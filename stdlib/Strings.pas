unit Strings;

interface

function Length(s: string): Integer;
function Copy(s: string; index, count: Integer): string;
function Pos(sub: string; s: string): Integer;
// More string functions

implementation

function Length(s: string): Integer;
external 'pas_strlen';

function Copy(s: string; index, count: Integer): string;
external 'pas_strcopy';

function Pos(sub: string; s: string): Integer;
external 'pas_strpos';

end.
