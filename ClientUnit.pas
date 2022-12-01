unit ClientUnit;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Win.ScktComp, Vcl.ExtCtrls,
  EncdDecd; //Encode-Decode Base64, builtin unit

type
  TMainForm = class(TForm)
    ConnectBtn: TButton;
    SendBtn: TButton;
    procedure ConnectBtnClick(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Socket:TClientSocket;
    DataTimer:TTimer;
    RandomData:string;
    procedure SocketConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
if not assigned(Socket) then begin
    Socket:=TClientSocket.Create(MainForm);
    Socket.Address:='127.0.0.1';
    Socket.Port:=49200;
    Socket.Active := True;
    Socket.OnConnect:=SocketConnect;
    Socket.OnDisconnect:=SocketDisconnect;
  end else begin
    Socket.Active:=false;
    Socket.Free;
    Socket:=nil;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
if Assigned(Socket) then begin
    Socket.Active:=false;
    Socket.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var i:word;
begin
RandomData:='';
For i:=1 to 10001 do begin
    RandomData:=RandomData+IntToStr(i mod 10);
  end;
SendBtn.Enabled:=false;
end;

procedure TMainForm.SendBtnClick(Sender: TObject);
begin
if Assigned(Socket) and Socket.Socket.Connected then
  Socket.Socket.SendText(EncodeString(Copy( RandomData,1,Random(9001)+1000 )));
end;

procedure TMainForm.SocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
ConnectBtn.Caption:='Disconnect';
SendBtn.Enabled:=true;
DataTimer:=TTimer.Create(MainForm);
DataTimer.OnTimer:=SendBtnClick;
DataTimer.Interval:=50;
DataTimer.Enabled:=true;
end;

procedure TMainForm.SocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
ConnectBtn.Caption:='Connect';
SendBtn.Enabled:=false;
DataTimer.Free;
end;

end.
