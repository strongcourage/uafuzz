include Cli.Make(
 struct
   let shortname = "ida"
   let name = "IDA Pro interface"
 end
)

module IdaOutputFile =
  Builder.String(
    struct
      let name = "o-ida"
      let default = "out.ida"
      let doc =  " Set IDA output file"
    end)

module IdaCfg =
  Builder.False(
      struct
        let name = "cfg-dot"
        let doc = " Generate CFGs in dot format"
      end
    )

module IdaSimpleCfg =
  Builder.True(
  struct
    let name = "simple"
    let doc = "Simple CFG containing basic blocks"
  end
  )
