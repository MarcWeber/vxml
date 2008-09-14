/* custom do rebinding >>=, >>, return lift
*/
#define vdo let (>>=) = vxmlbind; (>>) = vxmlgtgt; return = vxmlreturn; lift = vxmllift in do
/* Prelude do, resetting to default bindings (except lift)
   (I've tried giving it the name do using the token concatenation 
   d ## o to break recursion but this did only work for one define ?)
*/
#define pdo let (>>=) = (Prelude.>>=); (>>) = (Prelude.>>); return = Prelude.return; in do
