unit Operations;

interface

uses
  Classes, Types, Operation, DataType;

type
  IOperations = Interface(IInterface)
  ['{4BAAA81A-08BB-47C3-8BC4-FC6EE57F2B58}']
    function GetOperation(AOperation: string; ALeftType, ARightType: TDataType): TOperation;
  end;

implementation

end.
