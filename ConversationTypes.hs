module ConversationTypes where
    import qualified Data.Map as Map
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
        conversationStartMap :: Map.Map String String
    }

    instance Show Response where
        show = responseText

    instance Show ResponseSet where
        show = concatMap showResponse . zip [1..] . responses
            where
                showResponse (no, response) = show no ++ ") " ++ show response ++ "\n"

    instance Show Conversation where
        show Conversation { conversationText = text } = text
        show ConversationStopper { conversationStopperText = text } = text

    instance Entity Response where
        getId = responseId

    instance Entity ResponseSet where
        getId = responseSetId

    instance Entity Conversation where
        getId Conversation { conversationId = i } = i
        getId ConversationStopper { conversationStopperId = i } = i