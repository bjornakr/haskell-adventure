import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Entity
import Core
import Action.Core
import qualified Conversation

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

gameLoop :: GameState -> IO ()
gameLoop gamestate = do
    action <- getLine
    let actionResult = respondAction gamestate (parseAction action)
    case actionResult of
        ActionResult gamestate message -> do
            putStrLn message
            gameLoop gamestate
        ConversationTrigger gamestate conversationId -> do
            Conversation.main
            gameLoop gamestate
            
    --putStrLn (getMessageFromActionResult actionResult)
    --gameLoop (getGamestateFromActionResult actionResult)

main :: IO ()
main = do
    gameLoop (GameState samplePlayer sampleWorld sampleStateMap)
