structure EbnfExt = struct
    local
        val suffixes = ["g"]
        val class = "ebnf"
        fun sfx s =
            Tools.registerClassifier
                (Tools.stdSfxClassifier { sfx = s, class = class })
    in
        val _ = app sfx suffixes
    end
end
