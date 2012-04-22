unit CompilerDefines;

interface

type
  TMessageLevel = (mlNone, mlWarning, mlError, mlFatal);
  TOnMessage = procedure(AMessage, AUnitName: string; ALine: Integer; ALevel: TMessageLevel) of object;

implementation

end.
