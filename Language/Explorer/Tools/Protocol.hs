{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Language.Explorer.Tools.Protocol where

import GHC.Generics
import Data.Monoid
import Data.Aeson
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header (hContentType)
import Data.Maybe
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as S
import qualified Data.Attoparsec.ByteString.Lazy as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.Scientific
import qualified Language.Explorer.Monadic as Ex
import Control.Monad.RWS.Lazy hiding (listen)
import Control.Monad.Trans.Except
import Data.List

type ExplorerParser p m c o = (Ex.Explorer p m c o, String -> Maybe p)
type ProcessResult = Either ErrorMessage Value

type EIP p m c o = RWST (String -> Maybe p) S.ByteString (Ex.Explorer p m c o) m

class ExplorerPostValue p c o where
    postExecute :: Ex.Explorer p m c o -> Ex.Explorer p m c o -> o -> Value
    postExecute = \_ _ _ -> Null
    postJump :: Ex.Explorer p m c o -> Ex.Explorer p m c o -> Value
    postJump = \_ _ -> Null
    postRevert :: Ex.Explorer p m c o -> Ex.Explorer p m c o -> [Ex.Ref] -> Value
    postRevert = \ _ _ _ -> Null

data RequestMessage = RequestMessage {
    jsonrpc :: String,
    req_id :: String,
    method :: String,
    params :: Maybe Value
} deriving (Show, Generic)

instance ToJSON RequestMessage where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RequestMessage where
    parseJSON = withObject "RequestMessage" $ \v -> RequestMessage
        <$> v .: "jsonrpc"
        <*> v .: "id"
        <*> v .: "method"
        <*> v .:? "params"

data ResponseMessage = ResponseMessage {
    res_id :: String,
    body :: ProcessResult
} deriving (Show)

instance ToJSON ResponseMessage where
    toJSON (ResponseMessage res_id (Left e)) = object ["id" .= res_id, "error" .= e]
    toJSON (ResponseMessage res_id (Right res)) = object ["id" .= res_id, "result" .= res]

    toEncoding (ResponseMessage res_id (Left e)) = pairs ("id" .= res_id <> "error" .= e)
    toEncoding (ResponseMessage res_id (Right res)) = pairs ("id" .= res_id <> "result" .= res)


data ErrorMessage = ErrorMessage {
    code :: Int,
    message :: String,
    error_data :: Maybe Value
} deriving (Show, Generic)

instance ToJSON ErrorMessage where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ErrorMessage
    -- No need to provide a parseJSON implementation.

-- instance Except ErrorMessage 
-- where
--     noMsg = ErrorMessage { code = 0, message = "", error_data = Nothing}
--     strMsg msg = ErrorMessage {code = 0, message = msg, error_data = Nothing }

data JumpParams = JumpParams {
    jump_ref :: Int
}

instance FromJSON JumpParams where
    parseJSON = withObject "JumpParams" $ \v -> JumpParams
        <$> v .: "reference"

data JumpResult = JumpResult {
    jump_post :: Value
}

instance ToJSON JumpResult where 
    toJSON res = object ["post" .= jump_post res]

data ExecuteParams = ExecuteParams {
    paramsProgram :: String
} deriving (Show, Generic)

instance FromJSON ExecuteParams

data ExecuteResult = ExecuteResult {
    exec_ref :: Int,
    exec_out :: Value,
    exec_post :: Value
}

instance ToJSON ExecuteResult where
    toJSON (ExecuteResult ref out post) = object ["reference" .= ref, "output" .= out, "post" .= post]

    toEncoding (ExecuteResult ref out post) = pairs ("reference" .= ref <> "output" .= out <> "post" .= post)

data RevertParams = RevertParams {
    revert_ref :: Int
}

instance FromJSON RevertParams where
    parseJSON = withObject "RevertParams" $ \v -> RevertParams
        <$> v .: "reference"

data RevertResult = RevertResult {
    revert_deleted :: [Ex.Ref],
    post_revert :: Value
}

instance ToJSON RevertResult where 
    toJSON res = object ["deleted" .= revert_deleted res, "post" .= post_revert res]

data DerefParams = DerefParams {
    deref_ref :: Int
}

instance FromJSON DerefParams where
    parseJSON = withObject "DerefParams" $ \v -> DerefParams
        <$> v .: "reference"


data TraceParams = TraceParams {
    reference :: Int
} deriving (Generic)

instance FromJSON TraceParams


data Edge = Edge {
    source :: Int,
    target :: Int,
    label  :: EdgeLabel
} deriving (Generic)

instance ToJSON Edge where
    toEncoding = genericToEncoding defaultOptions


data EdgeLabel = EdgeLabel {
    program :: Value,
    mval :: Value
} deriving (Generic)

instance ToJSON EdgeLabel where
    toEncoding = genericToEncoding defaultOptions


data ExecutionTree = ExecutionTree {
    current :: Int,
    references :: [Int],
    edges :: [Edge]
} deriving (Generic)

instance ToJSON ExecutionTree where
    toEncoding = genericToEncoding defaultOptions

data PathParams = PathParams {
    paramsSource :: Int,
    paramsTarget :: Int
} deriving (Generic)

instance FromJSON PathParams

parseErrorCode = -32700
invalidRequestCode = -32600
methodNotFoundCode = -32601
invalidParamsCode = -32602
internalErrorCode = -32603

referenceNotInTreeCode = 1
referenceRevertInvalidCode = 2
programParseErrorCode = 3
pathNonExistingCode = 4



parseError :: ErrorMessage
parseError = ErrorMessage {
    code = parseErrorCode,
    message = "Parse error",
    error_data = Nothing
}

methodNotFound :: ErrorMessage
methodNotFound = ErrorMessage {
    code = methodNotFoundCode,
    message = "Method not found",
    error_data = Nothing
}

invalidParams :: ErrorMessage
invalidParams = ErrorMessage {
    code = invalidParamsCode,
    message = "Invalid method parameter(s)",
    error_data = Nothing
}

ensureParameter :: Monad m => Maybe Value -> ExceptT ErrorMessage (EIP p m c o) Value
ensureParameter Nothing = throwE invalidParams
ensureParameter (Just v) = return v

fromResult :: Monad m => Result a -> Value -> ExceptT ErrorMessage (EIP p m c o) Value
fromResult res onSuccess = case res of
    (Error e) -> throwE invalidParams
    (Success v) -> return onSuccess

jump :: (Monad m, ExplorerPostValue p c o) => Value -> ExceptT ErrorMessage (EIP p m c o) Value
jump v = case (fromJSON v) :: Result JumpParams of
    (Error e) -> throwE invalidParams
    (Success v') -> do
        ex <- lift $ get
        case Ex.jump (jump_ref v') ex of
            Just ex' -> do
                lift $ put $ ex'
                return . toJSON . JumpResult $ postJump ex ex'
            Nothing -> throwE ErrorMessage { code = referenceNotInTreeCode, message = "", error_data = Nothing }

execute :: (Eq o, Monoid o, ToJSON o, Eq p, ExplorerPostValue p c o) => Value -> ExceptT ErrorMessage (EIP p IO c o) Value
execute v = case (fromJSON v) :: Result ExecuteParams of
    (Error e) -> throwE invalidParams
    (Success v') -> do
        parser <- lift $ ask
        let pl = parser $ paramsProgram v'
        case pl of
            Just prog -> do
                ex <- lift $ get
                (ex', output) <- liftIO $ Ex.execute prog ex
                lift $ put ex'
                return $ toJSON $ ExecuteResult { exec_ref = Ex.currRef ex', exec_out = toJSON output, exec_post = postExecute ex ex' output }
            Nothing -> throwE ErrorMessage { code = programParseErrorCode, message = "", error_data = Nothing }

allRefs :: Ex.Explorer p IO c o -> [(Ex.Ref, c)]
allRefs ex = refs
    where
        (_, refs, _) = Ex.executionGraph ex


revert :: (Eq o, Monoid o, Eq p, ExplorerPostValue p c o) => Value -> ExceptT ErrorMessage (EIP p IO c o) Value
revert v = case (fromJSON v) :: Result RevertParams of
    (Error e) -> throwE invalidParams
    (Success v) -> do
        ex <- lift $ get
        case Ex.revert (revert_ref v) ex of
            Just ex' -> do
                lift $ put ex'
                return $ toJSON $ RevertResult { revert_deleted = deleted, post_revert = postRevert ex ex' deleted}
                where 
                    refs = map fst (allRefs ex)
                    refs' = map fst (allRefs ex')
                    deleted = (refs \\ refs')
            Nothing -> throwE ErrorMessage { code = referenceRevertInvalidCode, message = "", error_data = Nothing }

deref :: (Eq o, Monoid o, Eq p, ToJSON c) => Value -> ExceptT ErrorMessage (EIP p IO c o) Value
deref v = case (fromJSON v) :: Result DerefParams of
    (Error e) -> throwE invalidParams
    (Success v) -> do
        ex <- lift $ get
        case Ex.deref ex (deref_ref v) of
            (Just conf) -> return $ toJSON conf
            Nothing -> throwE ErrorMessage { code = referenceNotInTreeCode, message = "", error_data = Nothing}

executionTree :: (ToJSON o, ToJSON p) => ExceptT ErrorMessage (EIP p IO c o) Value
executionTree = do
    ex <- lift $ get
    let (curr, nodes, edges) = Ex.executionGraph ex
    return $ toJSON $ ExecutionTree 
        { current = fst curr
        , references = map fst nodes
        , edges = map (\(s, (p, o), t) -> Edge { source = fst s
        , label = EdgeLabel { program = toJSON p, mval = toJSON o}
        , target = fst t} ) edges}

getCurrentReference :: ExceptT ErrorMessage (EIP p IO c o) Value
getCurrentReference = do
    ex <- lift $ get
    return $ toJSON $ Ex.currRef ex

getAllReferences :: ExceptT ErrorMessage (EIP p IO c o) Value
getAllReferences = do
    ex <- lift $ get
    return $ toJSON $ map fst (allRefs ex)

getTrace :: (ToJSON p, ToJSON o) => Maybe Value -> ExceptT ErrorMessage (EIP p IO c o) Value
getTrace (Just r) = case (fromJSON r) :: Result TraceParams of
    (Error e) -> throwE invalidParams
    (Success v) -> do
        ex <- lift $ get
        let path = Ex.getPathFromTo ex 1 (reference (v :: TraceParams)) -- Fix hardcode 1(it's initialRef).
        return $ toJSON $ map (\(s, (p, o), t) -> Edge { source = fst s, target = fst t, label = EdgeLabel { program = toJSON p, mval = toJSON o} }) path
getTrace Nothing = do
    ex <- lift $ get
    let trace = Ex.getTrace ex
    return $ toJSON $ map (\(s, (p, o), t) -> Edge { source = fst s, target = fst t, label = EdgeLabel { program = toJSON p, mval = toJSON o} }) trace

getPath :: (ToJSON o, ToJSON p) => Value -> ExceptT ErrorMessage (EIP p IO c o) Value
getPath val = case (fromJSON val) :: Result PathParams of
    (Error e) -> throwE ErrorMessage { code = pathNonExistingCode, message = "", error_data = Nothing}
    (Success v) -> do
        ex <- lift $ get
        let path = Ex.getPathFromTo ex (paramsSource v) (paramsTarget v)
        return $ toJSON $ map (\(s, (p, o), t) -> Edge { source = fst s, target = fst t, label = EdgeLabel { program = toJSON p, mval = toJSON o} }) path

getLeaves :: ExceptT ErrorMessage (EIP p IO c o) Value
getLeaves = do
    ex <- lift $ get
    return $ toJSON $ map fst (Ex.leaves ex)

methodDispatch :: (Eq o, Monoid o, ToJSON o, ToJSON p, Eq p, ToJSON c, ExplorerPostValue p c o) => String -> Maybe Value -> ExceptT ErrorMessage (EIP p IO c o) Value
methodDispatch "jump" mval = ensureParameter mval >>= jump
methodDispatch "execute" mval = ensureParameter mval >>= execute
methodDispatch "revert" mval = ensureParameter mval >>= revert
methodDispatch "deref" mval = ensureParameter mval >>= deref
methodDispatch "getTrace" mval = getTrace mval
methodDispatch "getPath" mval = ensureParameter mval >>= getPath
methodDispatch "getExecutionTree" _ = executionTree
methodDispatch "getCurrentReference" _ = getCurrentReference
methodDispatch "getAllReferences" _ = getAllReferences
methodDispatch "getLeaves" _ = getLeaves
methodDispatch _ _ = throwE methodNotFound

handleRequest :: (Eq o, Monoid o, ToJSON o, ToJSON o, ToJSON p, Eq p, ToJSON c, ExplorerPostValue p c o) => Maybe RequestMessage -> EIP p IO c o ResponseMessage
handleRequest (Just msg) = do
    res <- runExceptT $ methodDispatch (method msg) (params msg)
    return $ ResponseMessage { res_id = req_id msg, body = res }
handleRequest Nothing = return $ ResponseMessage { res_id = "0", body = Left parseError { message = "NOthing" }}


handleRequest' :: (Eq o, Monoid o, ToJSON o, Eq p, ToJSON p, ToJSON c, ExplorerPostValue p c o) => S.ByteString -> EIP p IO c o ResponseMessage
handleRequest' body =
    case decode body of
        (Just m) -> handleRequest m
        Nothing -> return invalidHeader

invalidHeader :: ResponseMessage
invalidHeader = ResponseMessage { res_id = "0", body = Left parseError {message = "Headeer"} }

parseHeader :: AB.Parser (Int, String)
parseHeader = do
    AB.string "Content-Length:"
    AB.skipMany (ABC.char ' ')
    res <- ABC.scientific
    AB.skipMany (ABC.char ' ')
    ABC.char '\r'
    ABC.char '\n'
    ABC.string "Content-Type:"
    AB.skipMany (ABC.char ' ')
    typ <- AB.manyTill ABC.letter_ascii (ABC.char '\r')
    AB.skipMany (ABC.char ' ')
    ABC.char '\n'
    ABC.char '\r'
    ABC.char '\n'
    return $ (fromJust $ ((toBoundedInteger res) :: Maybe Int), typ)


intProg :: Int -> Int -> IO (Maybe Int, ())
intProg x y = do
    putStrLn . show $ y
    return (Just x, ())

intParse :: String -> Maybe Int
intParse _ = Just 1

-- TODO: Handle incorrect request.
-- TODO: Send correct error messages.
serve :: (Eq o, Monoid o, ToJSON o, Eq p, ToJSON p, ToJSON c, ExplorerPostValue p c o) => String -> Ex.Explorer p IO c o -> (String -> Maybe p) -> IO ()
serve port ex parser = withSocketsDo $ do
    addr <- resolve port
    E.bracket (open addr) close loop
  where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        fd <- fdSocket sock
        setCloseOnExecIfNeeded fd
        listen sock 10
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        forkFinally (talk ex parser conn) (\_ -> close conn)
    talk ex parser conn = do
        putStrLn "Hello receiving"
        msg <- recv conn 1024
        unless (S.null msg) $ do
            ex' <- acceptCommand ex parser conn msg
            talk ex' parser conn

acceptCommand ex parser conn command = do
    let res = AB.parse parseHeader command
    putStrLn $ show res
    (result, toParse) <- case res of
        (AB.Done rem (val, _)) -> do
            case S.length rem < (fromIntegral val) of
                True -> do
                    msg <- recv conn 1024
                    return (Nothing, S.append command msg)
                False -> do
                    let command = S.take (fromIntegral val) rem
                    putStrLn "-------------------------"
                    putStrLn $ show command
                    putStrLn "-------------------------"
                    out <- runRWST (handleRequest' command) parser ex
                    return (Just out, S.drop (fromIntegral val) rem)
        (AB.Fail _ _ "not enough input") -> do
            msg <- recv conn 1024
            return (Nothing, S.append command msg)
        _ ->  return (Just (invalidHeader, ex, ""), "")
    case result of
        Nothing -> if toParse == "" then return ex else acceptCommand ex parser conn toParse
        Just (resp, ex', log) -> do
            let encoded_resp = encode resp
            let full_resp = S.concat ["Content-Length:", encode $ S.length encoded_resp, "\r\nContent-Type: jrpcei\r\n\r\n", encoded_resp]
            sendAll conn full_resp
            putStrLn $ show full_resp
            putStrLn $ show toParse
            if toParse == "" then return ex' else acceptCommand ex' parser conn toParse
