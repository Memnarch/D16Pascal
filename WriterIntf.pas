unit WriterIntf;

interface

uses
  Classes, Types;

type
  IWriter = interface
    ['{EB297938-AE6F-47D7-9A25-F5A02507BEF4}']
    procedure Write(ALine: string);
    procedure WriteList(AList: TStrings);
    procedure AddMapping(AElement: TObject; AOffset: Integer = 0);
  end;

implementation

end.
