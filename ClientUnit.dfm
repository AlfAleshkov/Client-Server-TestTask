object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'TestTask Client'
  ClientHeight = 133
  ClientWidth = 212
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ConnectBtn: TButton
    Left = 8
    Top = 8
    Width = 193
    Height = 49
    Caption = 'Connect'
    TabOrder = 0
    OnClick = ConnectBtnClick
  end
  object SendBtn: TButton
    Left = 8
    Top = 63
    Width = 193
    Height = 58
    Caption = 'Send data'
    TabOrder = 1
    OnClick = SendBtnClick
  end
end
