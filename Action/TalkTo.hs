module Action.TalkTo where
    import Entity
    import Core

    talkTo :: GameState -> Actor -> ActionResult
    talkTo gamestate actor = ConversationTrigger gamestate "ROGER1"
