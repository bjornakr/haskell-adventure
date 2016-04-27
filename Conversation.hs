module Conversation where
    import Data.Char (digitToInt)
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


    data ConversationState = ConversationState [Conversation] [ResponseSet]

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

    showConversation :: ConversationState -> Conversation -> String
    showConversation (ConversationState _ conversationSets) conv@(Conversation { cResponseSetId = rsId }) =
        let responseSet = findEntityById rsId conversationSets in
            case responseSet of
                Nothing -> error $ "Could not find response set " ++ rsId
                (Just rs) -> (show conv) ++ "\n\n" ++ (show rs)

    getResponses :: ConversationState -> Conversation -> [Response]
    getResponses (ConversationState _ conversationSets) (Conversation { cResponseSetId = rsId }) =
        let responseSet = findEntityById rsId conversationSets in
            case responseSet of
                Nothing -> error $ "Could not find response set " ++ rsId
                (Just (ResponseSet { responses = r })) -> r
    getResponses _ (ConversationStopper {}) = []

    respond :: Response -> [Conversation] -> Maybe Conversation
    respond (Response { nextConversationId = cId }) conversations = findEntityById cId conversations

    num2resp :: Int -> [Response] -> Maybe Response
    num2resp x responses
        | x < 1 = Nothing
        | x > (length responses) = Nothing
        | otherwise = Just (responses !! (x-1))

    --h1 = Conversation { 
    --        conversationId = "H1",
    --        conversationText = "Hello. How are you doing?",
    --        cResponseSetId = "HR1"
    --    }

    --hr1 = ResponseSet {
    --        responseSetId = "HR1",
    --        responses = [
    --            Response "H11" "Great",
    --            Response "H11" "Not too bad",
    --            Response "H12" "Feeling down"
    --        ]
    --    }

    --h2 = Conversation {
    --        conversationId = "H11",
    --        conversationText = "That's good to hear.\n\n* He gives you an apple.",
    --        cResponseSetId = "HR2"
    --    }

    --hr2 = ResponseSet {
    --    responseSetId = "HR2",
    --    responses = [
    --        Response "HEND" "Good bye, I gotta run!",
    --        Response "HEND2" "*Run away*"
    --    ]
    --}


    --hend = ConversationStopper "HEND" "Ok, see you around!"

    --hend2 = ConversationStopper "HEND2" "* You run away before he can respond."

    a1 = Conversation { 
            conversationId = "A1",
            conversationText = "What can I do for you?",
            cResponseSetId = "AR1"
        }


    ar1 = ResponseSet {
            responseSetId = "AR1",
            responses = [
                Response "AR11" "tell-me-about-ruru" "Tell me about Ruru.",
                Response "AR12" "A2" "Would you like an apple?",
                Response "AR13" "AEND" "Goodbye."
            ]
        }

    a2 = Conversation {
        conversationId  = "A2",
        conversationText = "Thanks! *Munch munch*",
        cResponseSetId = "AR1"
    }

    a3 = Conversation {
        conversationId = "tell-me-about-ruru",
        conversationText = "He's a great guy!",
        cResponseSetId = "AR1"
    }
     



    removeResponse'' :: ResponseId -> ResponseSet -> ResponseSet
    removeResponse'' rId responseSet@(ResponseSet { responses = rs }) =
        responseSet { responses = (removeEntityById rId rs) }


    removeResponse' :: ResponseId -> [ResponseSet] -> [ResponseSet]
    removeResponse' _ [] = []
    removeResponse' rId (rs:rss) = (removeResponse'' rId rs):(removeResponse' rId rss)

    removeResponse :: ConversationState -> ResponseId -> ConversationState
    removeResponse (ConversationState conversations responseSets) rId = 
        ConversationState conversations (removeResponse' rId responseSets)

    addResponse' :: ResponseSetId -> Response -> [ResponseSet] -> [ResponseSet]
    addResponse' _ _ [] = []
    addResponse' rsId response (responseSet@(ResponseSet { responses = resps }):rss)
        | rsId == (getId responseSet) = (responseSet { responses = (response:resps) }):rss
        | otherwise = (responseSet:(addResponse' rsId response rss))

    addResponse :: ConversationState -> ResponseSetId -> Response -> ConversationState
    addResponse (ConversationState conversations responseSets) rsId response =    
        ConversationState conversations (addResponse' rsId response responseSets)

    replaceResponse :: ConversationState -> ResponseSetId -> ResponseId -> Response -> ConversationState
    replaceResponse state rsId rId newResponse = addResponse (removeResponse state rId) rsId newResponse


    processResponse :: ConversationState -> ResponseSetId -> ResponseId -> ConversationState
    processResponse state _ rId@"AR12" = removeResponse state rId

    processResponse state rsId rId@"AR11" = addResponse 
        (removeResponse state rId) 
        rsId
        (Response {
            responseId = "AR11b", 
            nextConversationId = "tell-me-about-ruru", 
            responseText = "What was that you said about Ruru again?" 
        })


    processResponse state _ rId@"ROGRES3a" = replaceResponse state "ROGRES3" rId rognew1
    processResponse state _ rId@"ROGRES3b" = addResponse (removeResponse state rId) "ROGRES3" rognew2

    processResponse state _ _ = state


    talkx state conv@(Conversation { cResponseSetId = rsId }) = do
        putStrLn $ showConversation state conv
        putStr "> "
        hFlush stdout
        choice <- readLn :: IO Int
        let response = num2resp choice (getResponses state conv)    
        case response of
            Nothing -> do
                talkx state conv
            Just resp@(Response { responseId = rId, nextConversationId = ncId }) -> do
                let newState = processResponse state rsId rId
                talk newState ncId


    talk :: ConversationState -> ConversationId -> IO ()
    talk state@(ConversationState conversations _) conversationId =
        let conversation = findEntityById conversationId conversations in
            case conversation of
                Nothing -> error $ "Could not find conversation id \"" ++ conversationId ++ "\"."
                Just conv@(ConversationStopper {}) -> putStrLn $ show conv
                Just conv -> talkx state conv


    main = do
        talk (ConversationState 
                [roger1, roger2, roger3, roger4, roger5, roger6, roger7, rogbye1, rogbye2, rogbye3] 
                [rogres1, rogres2, rogres3, rogres4, rogres5])
            "ROGER1"
        return ()


    roger1 = Conversation {
        conversationId = "ROGER1",
        conversationText = "I don't have anything to say to you.",
        cResponseSetId = "ROGRES1"
    }

    rogbye1 = ConversationStopper {
        conversationStopperId = "ROGBYE1",
        conversationStopperText = "I don't know anything about that. Why don't you take a hike?"
    }

    rogbye2 = ConversationStopper {
        conversationStopperId = "ROGBYE2",
        conversationStopperText = "Yeah, leave me alone."
    }

    roger2 = Conversation {
        conversationId = "ROGER2",
        conversationText = "What!? Where did you get that?",
        cResponseSetId = "ROGRES2"
    }

    roger3 = Conversation {
        conversationId = "ROGER3",
        conversationText = "Ok, ok... I will tell you what you need to know. Just please keep that photo to yourself, ok?",
        cResponseSetId = "ROGRES3"
    }

    roger4 = Conversation {
        conversationId = "ROGER4",
        conversationText = "Listen, I don't know anything... I was just told to deliver an envelope to Mrs. Kreuger.",
        cResponseSetId = "ROGRES4"
    }

    roger5 = Conversation {
        conversationId = "ROGER5",
        conversationText = "He said his name was Tony.",
        cResponseSetId = "ROGRES3"
    }

    roger6 = Conversation {
        conversationId = "ROGER6",
        conversationText = "I don't know, I swear. I... I visited her a week ago, and that's the last time I saw her.",
        cResponseSetId = "ROGRES5"
    }

    roger7 = Conversation {
        conversationId = "ROGER7",
        conversationText = "Listen, she didn't say anything out of the ordinary. Please, I don't know anything else.",
        cResponseSetId = "ROGRES3"
    }

    rogbye3 = ConversationStopper {
        conversationStopperId = "ROGERBYE3",
        conversationStopperText = "Please, just keep that photo to yourself."
    }

    rogres1 = ResponseSet {
        responseSetId = "ROGRES1",
        responses = [
            Response "ROGRES1a" "ROGBYE1" "Come on, I know you are involved with Ergo group!",
            Response "ROGRES1b" "ROGBYE1" "Dorothy is gone, and I intend to find her. What do you know about it?",
            Response "ROGRES1c" "ROGER2" "Why don't you take a look at this? * Show photo *",
            Response "ROGRES1d" "ROGBYE2" "Good bye."
        ]
    }

    rogres2 = ResponseSet {
        responseSetId = "ROGRES2",
        responses = [
            Response "ROGRES2a" "ROGER3" "I'm asking the questions now. Tell me, or I'll make sure this photo goes public."
        ]
    }

    rogres3 = ResponseSet {
        responseSetId = "ROGRES3",
        responses = [
            Response "ROGRES3a" "ROGER4" "What is your connection to the Ergo group?",
            Response "ROGRES3b" "ROGER6" "Where is Dorothy?",
            Response "ROGRES3c" "ROGERBYE3" "See you around."
        ]
    }

    rogres4 = ResponseSet {
        responseSetId = "ROGRES4",
        responses = [
            Response "ROGRES4a" "ROGER5" "Who told you to do that?"
        ]
    }

    rogres5 = ResponseSet {
        responseSetId = "ROGRES5",
        responses = [
            Response "ROGRES5a" "ROGER7" "Did she mention anything about Ergo group?"
        ]
    }

    rognew1 = Response "ROGNEW1" "ROGER5" "Who did you say your contact in Ergo group was?"
    rognew2 = Response "ROGNEW2" "ROGER7" "Are you sure Dorothy didn't mention anything unusual?"