object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'D16-Pascal -- defeating 45'
  ClientHeight = 643
  ClientWidth = 954
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnCompile: TButton
    Left = 407
    Top = 0
    Width = 75
    Height = 25
    Caption = 'btnCompile'
    TabOrder = 0
    OnClick = btnCompileClick
  end
  object Source: TSynEdit
    Left = 488
    Top = 0
    Width = 466
    Height = 643
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = SynPasSyn1
    Lines.Strings = (
      'unit Demo;'
      ''
      'var'
      #9'GRed: Word = 0x4000;'
      ''
      'procedure Print(AText: Word);'
      'var'
      #9'LVid: Word;'
      'begin'
      #9'LVid := 0x8000;'
      #9'while AText^  > 0 do'
      #9'begin'
      #9#9'LVid^ :=GRed + AText^;'
      #9#9'LVid := LVid + 1;'
      #9#9'AText := AText + 1;'
      #9'end;'
      'end;'
      ''
      'begin'
      #9'Print('#39'Hello world!'#39');'
      #9'asm'
      #9#9':halt '
      #9#9'set pc, halt'
      #9'end;'
      'end.')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces]
    TabWidth = 2
    WantTabs = True
  end
  object Target: TSynEdit
    Left = 0
    Top = 0
    Width = 401
    Height = 643
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = SynAsmSyn1
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces]
    TabWidth = 2
    WantTabs = True
  end
  object SynPasSyn1: TSynPasSyn
    KeyAttri.Foreground = clNavy
    Left = 344
    Top = 368
  end
  object SynAsmSyn1: TSynAsmSyn
    Left = 344
    Top = 256
  end
end
