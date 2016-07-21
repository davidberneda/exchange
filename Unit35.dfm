object FormExchangeTest: TFormExchangeTest
  Left = 0
  Top = 0
  ActiveControl = Memo1
  Caption = 'Exchange Test'
  ClientHeight = 466
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 32
    Top = 40
    Width = 513
    Height = 353
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 32
    Top = 420
    Width = 129
    Height = 25
    Caption = 'Test Inline only'
    TabOrder = 1
    OnClick = Button1Click
  end
end
