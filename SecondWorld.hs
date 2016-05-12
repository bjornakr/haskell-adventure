import qualified Data.Map as Map
import qualified Data.Set as Set
import Entity
import CoreTypes
import Core
import Action.Core
import ConversationTypes
import Conversation
import System.IO (hFlush, stdout)

bathroom =
    Room
        "Bathroom"
        [
            "Library"
        ]
        [
            LooseItem (ItemDetails "Toothbrush" "A sparkling new toothbrush."),
            StaticItem (ItemDetails "Toilet" "It's one of them fancy toilets."),
            StaticItem (ItemDetails "WashBasin" "A plain old wash basin.")
        ]
        [
            Actor "ToiletMan" [] "The great toilet man looms over thee."
        ]


library =
    Room 
        "Library"
        [          
            "Bathroom"
        ]
        [
            LooseItem (ItemDetails "Sword" "A beautiful, shining, freshly polished sword."),
            LooseItem (ItemDetails "Key" "It is a golden key."),
            LooseItem (ItemDetails "Book" "The title is Zob Goblin and the Poggle of Buckletwig.")
        ]
        [
            Actor "YoungLad" [LooseItem (ItemDetails "Apple" "A red, shining apple.")] "A fine, young lad."
        ]

jail = Room "Jail" 
    [] -- no exits initially, will exit to library when door is open
    [
        LooseItem (ItemDetails "JaildoorKey" "A rusty, iron key."),
        StaticItem (ItemDetails "Jaildoor" "It's a steel bar door, impossible to break."),
        StaticItem (ItemDetails "Box" "It's a closed box.")
    ]
    []

sampleWorld = [library, bathroom, jail]
samplePlayer = Player "Bathroom" [LooseItem (ItemDetails "Gun" "It's a good, old Smith & Wesson.")]
sampleStateMap = Map.singleton "Jaildoor" (Set.singleton "") --  [("Jaildoor", ["Locked"])]

convState = ConversationState
                [roger1, roger2, roger3, roger4, roger5, roger6, roger7, rogbye1, rogbye2, rogbye3] 
                [rogres1, rogres2, rogres3, rogres4, rogres5]                
                (Map.fromList [("ROGER", "ROGER1")])

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





gameLoop :: GameState -> IO ()
gameLoop gamestate = do
    putStrLn $ 
        "\nWhat would you like to do?\n" ++
        "[W]alk to | [L]ook (at) | [P]ick up | Co[M]bine\n" ++
        "[G]ive    | [T]alk to   | Pu[S]h    | Pu[L]l   \n" ++
        "[O]pen    | [C]lose     | [U]se     |          \n"
    putStr "> "
    hFlush stdout

    action <- getLine
    let actionResult = respondAction action gamestate
    gs <- case actionResult of
        ActionResult message gamestate -> do
            putStrLn message
            return gamestate
        ConversationTrigger conversationId gamestate -> 
            initiateConversation conversationId gamestate            
    gameLoop gs

main :: IO ()
main = do
    gameLoop (GameState {
        player = samplePlayer,
        world = sampleWorld,
        stateMap = sampleStateMap,
        conversationState = convState
        })
