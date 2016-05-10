module Conversation where
    import Data.Char (digitToInt)
    import qualified Data.Map as Map
    import System.IO (hFlush, stdout)
    import Entity

    type ConversationId = String
    type ResponseSetId = String
    type ResponseId = String

    data Response = Response {
        responseId :: String,
        nextConversationId :: String,
        responseText :: String
    } deriving (Eq)

    data ResponseSet = ResponseSet {
        responseSetId :: String,
        responses :: [Response]
    } deriving (Eq)

    data Conversation = 
        Conversation {
            conversationId :: String,
            conversationText :: String,
            cResponseSetId :: String
        } 
        | ConversationStopper {
            conversationStopperId :: String,
            conversationStopperText :: String
        } deriving (Eq)


    data ConversationState = ConversationState {
        stateConversations :: [Conversation],
        stateResponseSets :: [ResponseSet],
        conversationStartMap :: (Map.Map String String)
    }

    instance Show Response where
        show r = responseText r

    instance Show ResponseSet where
        show ResponseSet { responses = rs } = (concat (map showResponse (zip [1..] rs)))
            where showResponse (no, response) = (show no) ++ ") " ++ show response ++ "\n"

    instance Show Conversation where
        show (Conversation { conversationText = text }) = text
        show (ConversationStopper { conversationStopperText = text }) = text

    instance Entity Response where
        getId (Response { responseId = i }) = i

    instance Entity ResponseSet where
        getId (ResponseSet { responseSetId = i }) = i

    instance Entity Conversation where
        getId (Conversation { conversationId = i }) = i
        getId (ConversationStopper { conversationStopperId = i }) = i

    showConversation :: Conversation -> ConversationState -> String
    showConversation conv@(Conversation { cResponseSetId = rsId }) (ConversationState { stateResponseSets = responseSets }) =
        case (findEntityById rsId responseSets) of
            Nothing -> error $ "Could not find response set " ++ rsId
            (Just rs) -> (show conv) ++ "\n\n" ++ (show rs)

    getResponses :: Conversation -> ConversationState -> [Response]
    getResponses (Conversation { cResponseSetId = rsId }) (ConversationState { stateResponseSets = responseSets }) =
        case (findEntityById rsId responseSets) of
            Nothing -> error $ "Could not find response set " ++ rsId
            (Just (ResponseSet { responses = r })) -> r
    getResponses (ConversationStopper {}) _ = []

    respond :: Response -> [Conversation] -> Maybe Conversation
    respond (Response { nextConversationId = cId }) = findEntityById cId

    num2resp :: Int -> [Response] -> Maybe Response
    num2resp x responses
        | x < 1 = Nothing
        | x > (length responses) = Nothing
        | otherwise = Just (responses !! (x-1))

    removeResponse :: ResponseId -> ConversationState -> ConversationState
    removeResponse rId state@(ConversationState { stateResponseSets = responseSets }) = 
        state { stateResponseSets = map (remove rId) responseSets }
        where
            remove :: ResponseId -> ResponseSet -> ResponseSet
            remove rId responseSet@(ResponseSet { responses = rs }) =
                responseSet { responses = (removeEntityById rId rs) }

    addResponse :: ResponseSetId -> Response -> ConversationState -> ConversationState
    addResponse rsId response state@(ConversationState { stateResponseSets = responseSets }) =
        state { stateResponseSets = map (add rsId response) responseSets }
        where
            add :: ResponseSetId -> Response -> ResponseSet -> ResponseSet
            add rsId response responseSet@(ResponseSet { responses = resps })
                | rsId == (getId responseSet) = responseSet { responses = (response:resps) }
                | otherwise = responseSet

    replaceResponse :: ResponseSetId -> ResponseId -> Response -> ConversationState -> ConversationState
    replaceResponse rsId rId newResponse = addResponse rsId newResponse . removeResponse rId

    insertInConversationStartMap :: String -> String -> ConversationState -> ConversationState
    insertInConversationStartMap key val state@(ConversationState { conversationStartMap = sm }) = 
        state { conversationStartMap = Map.insert key val sm }


    processResponse :: ResponseSetId -> ResponseId -> ConversationState -> ConversationState
    processResponse rsId rId =
        case rId of
            "ROGRES1c" -> insertInConversationStartMap "ROGER" "ROGER3"
            "ROGRES3a" -> replaceResponse "ROGRES3" rId rognew1
            "ROGRES3b" -> addResponse "ROGRES3" rognew2 . removeResponse rId            
            _ -> id
        where 
            rognew1 = Response "ROGNEW1" "ROGER5" "Who did you say your contact in Ergo group was?"
            rognew2 = Response "ROGNEW2" "ROGER7" "Are you sure Dorothy didn't mention anything unusual?"

    talkx conv@(Conversation { cResponseSetId = rsId }) state = do
        putStrLn $ showConversation conv state
        putStr "> "
        hFlush stdout
        choice <- readLn :: IO Int
        let response = num2resp choice (getResponses conv state)
        case response of
            Nothing -> do
                talkx conv state
            Just resp@(Response { responseId = rId, nextConversationId = ncId }) -> do
                let newState = processResponse rsId rId state
                talk ncId newState


    talk :: ConversationId -> ConversationState -> IO ConversationState
    talk conversationId state@(ConversationState {stateConversations = conversations }) =
        case (findEntityById conversationId conversations) of
            Nothing -> error $ "Could not find conversation id \"" ++ conversationId ++ "\"."
            Just conv@(ConversationStopper {}) -> (putStrLn $ show conv) >> return state
            Just conv -> talkx conv state


