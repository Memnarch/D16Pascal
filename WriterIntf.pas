unit WriterIntf;

interface

type
  IWriter = interface
    ['{EB297938-AE6F-47D7-9A25-F5A02507BEF4}']
    procedure Write(ALine: string);
    procedure AddMapping();
  end;

implementation

end.
