unit Math;

interface

function Sin(x: Real): Real;
function Cos(x: Real): Real;
function Sqrt(x: Real): Real;
// More math functions

implementation

function Sin(x: Real): Real;
external 'pas_sin';

function Cos(x: Real): Real;
external 'pas_cos';

function Sqrt(x: Real): Real;
external 'pas_sqrt';

end.
