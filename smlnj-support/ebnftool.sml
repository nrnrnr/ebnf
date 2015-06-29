structure EbnfTool = struct
    local
        val tool = "EBNF"
        val kw_sigopts = "sigoptions"
        val kw_smlopts = "smloptions"
        val kwl = [kw_sigopts, kw_smlopts]
        (* This is a bit clumsy because we call parseOptions twice.
         * However, this is not really such a big deal in practice... *)
        fun get kw NONE = NONE
          | get kw (SOME opts) =
            #matches (Tools.parseOptions
                          { tool = tool, keywords = kwl, options = opts }) kw
    in
        val _ = Tools.registerStdShellCmdTool
                    { tool = tool,
                      class = "ebnf",
                      cmdStdPath = fn () => ("ebnf", ["-ml", "-picky"]),
                      template = SOME "%c -tabfilename %2t -ml %s | /home/nr/noweb/dist/contrib/norman/generate-to %1t",
                      extensionStyle =
                      Tools.EXTEND [("sml",      SOME "sml", get kw_smlopts)
                                   ,("-tab.sml", SOME "sml", get kw_smlopts)
                                   ],
                      dflopts = [] }
    end
end
