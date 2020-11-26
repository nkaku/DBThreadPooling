unit untPool;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Phys.MySQLDef, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.MySQL, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  System.Threading, System.Classes;

type
  TCallback = procedure(s: string) of object;

function OpenSQL(sSQL: string; pCallback: TCallback = nil): TFDQuery;
function ExecSQL(sSQL: string; pCallback: TCallback = nil): Integer;

implementation

uses
  System.SysUtils;

type
  TSQLType = (stOpen, stExecute);

  TPooledQuery = class(TThread)
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
    FSQL: string;
    FResume: Boolean;
    FShutdown: Boolean;
    FSQLType: TSQLType;
    procedure SetSQL(const Value: string);
  protected
    procedure Execute; override;
  public
    CallBack: TCallback;

    procedure Initialise;
    procedure Finalise;
    property Connection: TFDConnection read FConnection;
    property Query: TFDQuery read FQuery;
    property SQL: string read FSQL write SetSQL;
    property SQLType: TSQLType write FSQLType;
  end;

var
  Queries: TArray<TPooledQuery>;
  LastUsed: Integer;

function ExecSQL(sSQL: string; pCallback: TCallback = nil): Integer;
begin
  Inc(LastUsed);
  if LastUsed = Length(Queries) then
    LastUsed := 0;

  Queries[LastUsed].CallBack := pCallback;
  Queries[LastUsed].SQLType := stExecute;
  Queries[LastUsed].SQL := sSQL;
  if Queries[LastUsed].Suspended then
    Queries[LastUsed].Start;

  Result := Queries[LastUsed].Query.RowsAffected;
end;

function OpenSQL(sSQL: string; pCallback: TCallback = nil): TFDQuery;
begin
  Inc(LastUsed);
  if LastUsed = Length(Queries) then
    LastUsed := 0;

  Queries[LastUsed].CallBack := pCallback;
  Queries[LastUsed].SQLType := stOpen;
  Queries[LastUsed].SQL := sSQL;
  if Queries[LastUsed].Suspended then
    Queries[LastUsed].Start;

  Result := Queries[LastUsed].Query;
end;

procedure CreatePool(NoOfWorkers: Integer);
var
  PQ: TPooledQuery;
  I: Integer;
begin
  SetLength(Queries, NoOfWorkers);
  for I := 0 to NoOfWorkers - 1 do
  begin
    PQ := TPooledQuery.Create(True);
    PQ.Initialise;
    Queries[I] := PQ;
  end;
end;


procedure DestroyPool;
var
  I: Integer;
begin
  for I := 0 to Length(Queries) - 1 do
  begin
    Queries[I].FShutdown := True;
    Queries[I].Finalise;
    Queries[I].Terminate;
    Queries[I].Free;
  end;

  SetLength(Queries, 0);
end;

{ TPooledQuery }

procedure TPooledQuery.Execute;
begin
  inherited;

  while True do
  begin
    if FResume then
    begin
      if FSQLType = stOpen then
      begin
        FQuery.Close;
        FQuery.Open(FSQL);
      end
      else
        FQuery.ExecSQL(FSQL);

      Synchronize(
        procedure
        begin
          if Assigned(CallBack) then
            CallBack(IntToStr(LastUsed)+'-'+FSQL);
        end);
      FResume := False;
    end;

    if FShutdown then
      break;

    Sleep(5);
  end;

end;

procedure TPooledQuery.Finalise;
begin
  if Assigned(FQuery) then
  begin
    FQuery.Close;
    FQuery.Free;
  end;

  if Assigned(FConnection) then
  begin
    FConnection.Close;
    FConnection.Free;
  end;
end;

procedure TPooledQuery.Initialise;
begin
  if not Assigned(FConnection) then
  begin
    FConnection := TFDConnection.Create(nil);
    FConnection.Params.Add('Database=bidsbay');
    FConnection.Params.Add('User_Name=root');
    FConnection.Params.Add('Server=localhost');
    FConnection.Params.Add('DriverID=MySQL');
    FConnection.ResourceOptions.KeepConnection := True;
    FConnection.ResourceOptions.AutoReconnect := True;
    FConnection.LoginPrompt := False;
    FConnection.Connected := True;
  end;

  if not Assigned(FQuery) then
  begin
    FQuery := TFDQuery.Create(FConnection);
    FQuery.Connection := FConnection;
  end;
end;

procedure TPooledQuery.SetSQL(const Value: string);
begin
  FSQL := Value;
  FResume := True;
end;

initialization
  LastUsed := -1;
  CreatePool(5);

finalization
  DestroyPool;

end.