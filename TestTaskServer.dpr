program TestTaskServer;

{$APPTYPE CONSOLE}

uses
{$IFDEF LINUX}
  WinUtils, libc,
{$ENDIF}
  Winapi.Windows, System.SysUtils,  System.Classes, System.Win.ScktComp,
  SyncObjs, // For Critical sections, to avoid race conditions of "WriteLn"
  EncdDecd, // Base64 encode/decode builtin unit
  SQLite3, SQLite3Wrap; // SQLite3 as DataBase

type
  EServerThread = class(Exception);
  TServerThread = class(TServerClientThread)
  private
    fSocketStream : TWinSocketStream;
  public
    procedure ClientExecute; override;
  end;

  TMainThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Srv:TServerSocket;
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    procedure fSocketGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket;
      var SocketThread: TServerClientThread);
  end;

var
  MainThread:TMainThread;
  cs:TCriticalSection;
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;

{ TMainThread }

constructor TMainThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  Srv:=TServerSocket.Create(nil);
  //Srv.OnClientConnect:=ClientConnect;
  //Srv.OnClientDisconnect:=ClientDisconnect;
  //Srv.OnClientRead:=ClientRead;
  Srv.Port:=49200;
  Srv.OnGetThread:=fSocketGetThread;
  Srv.ServerType:=stThreadBlocking;
end;

destructor TMainThread.Destroy;
begin
  Srv.Free;
  inherited;
end;

procedure TMainThread.Execute;
var
  Msg: TMsg;
begin
  inherited;
  Srv.Active:=true;
  FreeOnTerminate := false;
  while not Terminated do
  begin
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
end;

procedure TMainThread.fSocketGetThread(Sender: TObject;
  ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);
begin
  cs.Enter;
  WriteLn('Client connect. Id:'+IntToStr(ClientSocket.SocketHandle));
  cs.Leave;
  SocketThread := TServerThread.Create( false, ClientSocket );
end;

{ TServerThread }

procedure TServerThread.ClientExecute;
var
  incomingData:string;
  Data: array[0..10000] of AnsiChar;
  NumRead:Integer;
begin
  inherited FreeOnTerminate := true;
  try
    fSocketStream := TWinSocketStream.Create( ClientSocket, 30000 );
    try
      while ( not Terminated ) and ClientSocket.Connected do
        try
          if not fSocketStream.WaitForData(5000) then Continue;
          NumRead := fSocketStream.Read(Data, SizeOf(Data));
          if NumRead = 0 then Exit;
          incomingData:=incomingData+Copy(Data,0,NumRead);
          if ClientSocket.ReceiveLength = 0 then begin
            incomingData:=DecodeString(incomingData);
            cs.Enter;
              WriteLn('Data recieved. Id:'+IntToStr(ClientSocket.SocketHandle)+' Length:'+IntToStr(Length(incomingData)));
              WriteLn(Copy(incomingData,1,40)+'...'); //first 40 chars of incomming data
              //IBQuery1.SQL.Clear;
              //IBQuery1.SQL.Add('INSERT INTO TestTaskTable(client_id,data) VALUES ('+IntToStr(ClientSocket.SocketHandle)',"'+incomingData+'")')
              //IBQuery1.ExecSQL;
              Stmt := DB.Prepare('INSERT INTO TestTask (id, data) VALUES (?, ?)');
              try
                Stmt.BindInt(1,ClientSocket.SocketHandle);
                Stmt.BindText(2,incomingData);
                Stmt.StepAndReset;
              finally
                Stmt.Free;
              end;
            cs.Leave;
            incomingData:='';
            end;
        except
          on e:exception do
          begin
            ClientSocket.Close;
            Terminate;
          end;
        end;
    finally
      fSocketStream.Free;
      cs.Enter;
      WriteLn('Client disconnect. Id:'+IntToStr(ClientSocket.SocketHandle));
      cs.Leave;
    end;
  except
    on e:exception do
    begin
      ClientSocket.Close;
      Terminate;
    end;
  end;
end;

begin
ReportMemoryLeaksOnShutdown:=true;
  try
    WriteLn('TestTask Server program');
    try
      MainThread:=TMainThread.Create(false);
      cs:=TCriticalSection.Create;
      DB := TSQLite3Database.Create;
      DeleteFile('TestTask.db');
      DB.Open('TestTask.db');
      DB.Execute('CREATE TABLE TestTask (id INTEGER, data TEXT)');
      ReadLn;
    finally
      FreeAndNil(MainThread);
      cs.Free;
      DB.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
