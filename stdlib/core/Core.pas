unit Core;

interface

procedure WriteLn(s: string);
function ReadLn: string;
// More core functions

implementation

procedure WriteLn(s: string);
external 'pas_writeln' library 'poscal-rs_runtime';

function ReadLn: string;
external 'pas_readln' library 'poscal-rs_runtime';

// Implementations if needed

end.
