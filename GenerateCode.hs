import Language.Copilot
import Copilot.Compile.C99
import Paxos

main = reify paxos >>= compile defaultParams
