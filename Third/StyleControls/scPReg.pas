{*******************************************************************}
{                                                                   }
{       Almediadev Visual Component Library                         }
{       StyleControls                                               }
{       Version 4.62                                                }
{                                                                   }
{       Copyright (c) 2014-2020 Almediadev                          }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{       Home:  http://www.almdev.com                                }
{       Support: support@almdev.com                                 }
{                                                                   }
{*******************************************************************}

unit scPReg;

interface
  uses
    Classes, SysUtils, Controls, DesignEditors, DesignIntf, scControls, scShellControls,
      scImageCollection, scAdvancedControls, scExtControls, scCalendar, TypInfo,
      Vcl.Graphics, scBitmapEditor, scAppPager, scToolPager, scAdvancedPager,
      scStyleManager, scStyledForm, scHint, scModernControls, scWebBrowser,
      scGrids, scDBControls, scDBGrids, scHtmlControls,scColorControls,
      scSelectPathDialog, scOpenFileDialog, scOpenPictureDialog,
      scColorDialog, scFontDialog, scPrinterDialog,
      scGPControls, scGPMeters, scGPImages,
      scGPExtControls, scGPPagers, scGPVertPagers, scGPDBControls,
      scGPFontControls, scFontViewer;

  type

    TscPageControlEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscBitmapPropertyEditor = class(TStringProperty)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: String; override;
    end;

    TscGPBitmapPropertyEditor = class(TStringProperty)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: String; override;
    end;

    TscGPPngPropertyEditor = class(TStringProperty)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: String; override;
    end;

    TscGPCharPropertyEditor = class(TStringProperty)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: String; override;
    end;

    TscGPCharImagePropertyEditor = class(TStringProperty)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: String; override;
    end;


    TscPngPropertyEditor = class(TStringProperty)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: String; override;
    end;

    TscGPPageControlEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscGPVertPageControlEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscGPToolPagerEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscAppPagerEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscToolPagerEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscAdvancedPagerEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscPageViewerEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscGPPageViewerEditor = class(TDefaultEditor)
    public
      procedure ExecuteVerb(Index: Integer); override;
      function GetVerb(Index: Integer): string; override;
      function GetVerbCount: Integer; override;
    end;

    TscDBStringProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      procedure GetValueList(List: TStrings); virtual;
      procedure GetValues(Proc: TGetStrProc); override;
    end;

    TscColumnDataFieldProperty = class(TscDBStringProperty)
      procedure GetValueList(List: TStrings); override;
    end;

    TscDBLookUpComboBoxFieldProperty = class(TscDBStringProperty)
      procedure GetValueList(List: TStrings); override;
    end;

     TscSwitchEditor = class(TDefaultEditor)
     protected
       procedure EditProperty(const PropertyEditor: IProperty;
         var Continue: Boolean); override;
     end;

     TscAdvancedListEditor = class(TDefaultEditor)
     protected
       procedure EditProperty(const PropertyEditor: IProperty;
         var Continue: Boolean); override;
     end;

    procedure Register;

implementation
  Uses DB, PngImage;
 {$R *.dcr}

resourcestring
  sNEW_PAGE = 'New page';
  sDEL_PAGE = 'Delete page';
  sPRIOR_PAGE = 'Prior page';
  sNEXT_PAGE = 'Next page';

procedure TscAdvancedListEditor.EditProperty;
begin
  if PropertyEditor.GetName = 'OnItemClick' then
    inherited;
end;

procedure TscSwitchEditor.EditProperty;
begin
  if PropertyEditor.GetName = 'OnChangeState' then
    inherited;
end;

function TscDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TscDBStringProperty.GetValueList(List: TStrings);
begin
end;

procedure TscDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TscDBLookUpComboBoxFieldProperty.GetValueList(List: TStrings);
var
  DataSource: TDataSource;
begin
  DataSource := (GetComponent(0) as TscDBLookUpComboBox).ListSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.GetFieldNames(List);
end;

procedure TscColumnDataFieldProperty.GetValueList(List: TStrings);
var
  Grid: TscCustomDBGrid;
  DataSource: TDataSource;
begin
  Grid := (GetComponent(0) as TscColumn).Grid;
  if (Grid = nil) then Exit;
  DataSource := Grid.DataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.GetFieldNames(List);
end;

function TscGPPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
  end;
end;

procedure TscGPPageControlEditor.ExecuteVerb(Index: Integer);
var
  Control: TscGPPageControl;
begin
  if Component is TscGPPageControl
  then
    Control :=  TscGPPageControl(Component)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0:
      begin
        Control.Tabs.Add;
      end;
      1:
      begin
        if Control.ActivePage <> nil then Control.ActivePage.Free;
      end;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscGPPageControlEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TscGPVertPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
  end;
end;

procedure TscGPVertPageControlEditor.ExecuteVerb(Index: Integer);
var
  Control: TscGPVertPageControl;
begin
  if Component is TscGPVertPageControl
  then
    Control :=  TscGPVertPageControl(Component)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0:
      begin
        Control.Tabs.Add;
      end;
      1:
      begin
        if Control.ActivePage <> nil then Control.ActivePage.Free;
      end;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscGPVertPageControlEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TscGPToolPagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
  end;
end;

procedure TscGPToolPagerEditor.ExecuteVerb(Index: Integer);
var
  Control: TscGPToolPager;
begin
  if Component is TscGPToolPager
  then
    Control :=  TscGPToolPager(Component)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0:
      begin
        Control.Tabs.Add;
      end;
      1:
      begin
        if Control.ActivePage <> nil then Control.ActivePage.Free;
      end;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscGPToolPagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TscAppPagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
  end;
end;

procedure TscAppPagerEditor.ExecuteVerb(Index: Integer);
var
  Control: TscAppPager;
begin
  if Component is TscAppPager
  then
    Control :=  TscAppPager(Component)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0: Control.CreatePage;
      1: if Control.ActivePage <> nil then Control.ActivePage.Free;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscAppPagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TscToolPagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
  end;
end;

procedure TscToolPagerEditor.ExecuteVerb(Index: Integer);
var
  Control: TscToolPager;
begin
  if Component is TscToolPager
  then
    Control :=  TscToolPager(Component)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0:
      begin
        Control.Tabs.Add;
      end;
      1:
      begin
        if Control.ActivePage <> nil then Control.ActivePage.Free;
      end;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscToolPagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


function TscAdvancedPagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
  end;
end;

procedure TscAdvancedPagerEditor.ExecuteVerb(Index: Integer);
var
  Control: TscAdvancedPager;
begin
  if Component is TscAdvancedPager
  then
    Control :=  TscAdvancedPager(Component)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0:
      begin
        Control.Tabs.Add;
      end;
      1:
      begin
        if Control.ActivePage <> nil then Control.ActivePage.Free;
      end;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscAdvancedPagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TscPageViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
    2: Result := sNext_PAGE;
    3: Result := sPrior_PAGE;
  end;
end;

procedure TscPageViewerEditor.ExecuteVerb(Index: Integer);
var
  Control: TscPageViewer;
begin
  if Component is TscPageViewer
  then
    Control :=  TscPageViewer(Component)
  else
  if (Component is TscPageViewerPage) and
     (TscPageViewerPage(Component).Parent is TscPageViewer) then
    Control := TscPageViewer(TscPageViewerPage(Component).Parent)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0:
      begin
        Control.Pages.Add;
      end;
      1:
      begin
        if Control.ActivePage <> nil then Control.ActivePage.Free;
      end;
      2:
      begin
        Control.PageIndex := Control.PageIndex + 1;
      end;
      3:
      begin
        Control.PageIndex := Control.PageIndex - 1;
      end;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscPageViewerEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

function TscGPPageViewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
    2: Result := sNext_PAGE;
    3: Result := sPrior_PAGE;
  end;
end;

procedure TscGPPageViewerEditor.ExecuteVerb(Index: Integer);
var
  Control: TscGPPageViewer;
begin
  if Component is TscGPPageViewer
  then
    Control :=  TscGPPageViewer(Component)
  else
  if (Component is TscGPPageViewerPage) and
     (TscGPPageViewerPage(Component).Parent is TscGPPageViewer) then
    Control := TscGPPageViewer(TscGPPageViewerPage(Component).Parent)
  else
    Control := nil;
  if Control <> nil
  then
    case Index of
      0:
      begin
        Control.Pages.Add;
      end;
      1:
      begin
        if Control.ActivePage <> nil then Control.ActivePage.Free;
      end;
      2:
      begin
        Control.PageIndex := Control.PageIndex + 1;
      end;
      3:
      begin
        Control.PageIndex := Control.PageIndex - 1;
      end;
    end;
  if Designer <> nil then Designer.Modified;
end;

function TscGPPageViewerEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

procedure RewritePngPublishedProperty(ComponentClass: TClass; const PropertyName: string);
var
   LPropInfo: PPropInfo;
begin
  LPropInfo := GetPropInfo(ComponentClass, PropertyName);
  if LPropInfo <> nil
  then
    RegisterPropertyEditor(LPropInfo^.PropType^, ComponentClass, PropertyName, TscPngPropertyEditor);
end;

procedure GPRewritePngPublishedProperty(ComponentClass: TClass; const PropertyName: string);
var
   LPropInfo: PPropInfo;
begin
  LPropInfo := GetPropInfo(ComponentClass, PropertyName);
  if LPropInfo <> nil
  then
    RegisterPropertyEditor(LPropInfo^.PropType^, ComponentClass, PropertyName, TscGPPngPropertyEditor);
end;

procedure GPRewriteCharPublishedProperty(ComponentClass: TClass; const PropertyName: string);
var
   LPropInfo: PPropInfo;
begin
  LPropInfo := GetPropInfo(ComponentClass, PropertyName);
  if LPropInfo <> nil
  then
    RegisterPropertyEditor(LPropInfo^.PropType^, ComponentClass, PropertyName, TscGPCharPropertyEditor);
end;

procedure GPRewriteCharImagePublishedProperty(ComponentClass: TClass; const PropertyName: string);
var
   LPropInfo: PPropInfo;
begin
  LPropInfo := GetPropInfo(ComponentClass, PropertyName);
  if LPropInfo <> nil
  then
    RegisterPropertyEditor(LPropInfo^.PropType^, ComponentClass, PropertyName, TscGPCharImagePropertyEditor);
end;

procedure GPRewriteBitmapPublishedProperty(ComponentClass: TClass; const PropertyName: string);
var
   LPropInfo: PPropInfo;
begin
  LPropInfo := GetPropInfo(ComponentClass, PropertyName);
  if LPropInfo <> nil
  then
    RegisterPropertyEditor(LPropInfo^.PropType^, ComponentClass, PropertyName, TscGPBitmapPropertyEditor);
end;

procedure RewriteBitmapPublishedProperty(ComponentClass: TClass; const PropertyName: string);
var
   LPropInfo: PPropInfo;
begin
  LPropInfo := GetPropInfo(ComponentClass, PropertyName);
  if LPropInfo <> nil
  then
    RegisterPropertyEditor(LPropInfo^.PropType^, ComponentClass, PropertyName, TscBitmapPropertyEditor);
end;

function TscPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:  result := sNEW_PAGE;
    1:  result := sDEL_PAGE;
  end;
end;

procedure TscPageControlEditor.ExecuteVerb(Index: Integer);
var
  NewPage: TscTabSheet;
  PControl : TscPageControl;
begin
  if Component is TscPageControl then
    PControl := TscPageControl(Component)
  else PControl := TscPageControl(TscTabSheet(Component).PageControl);
  case Index of
    0:  begin
          NewPage := TscTabSheet.Create(Designer.GetRoot);
          with NewPage do
          begin
            if PControl.HideTabs then
              TabVisible := False;
            Parent := PControl;
            PageControl := PControl;
            Name := Designer.UniqueName(ClassName);
            Caption := Name;
          end;
          if PControl.HideTabs then
            PControl.ActivePage := NewPage;
        end;
    1:  begin
          with PControl do
          begin
            NewPage := TscTabSheet(ActivePage);
            NewPage.PageControl := nil;
            NewPage.Free;
          end;
        end;
  end;
  if Designer <> nil then Designer.Modified;
end;

function TscPageControlEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TscGPBitmapPropertyEditor.Edit;
begin
  if PropCount <> 1 then Exit;
  ExecutePngToBitmapEditor(TscGPImageCollectionItem(GetComponent(0)).Bitmap, False);
  Designer.Modified;
end;

function TscGPBitmapPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TscGPBitmapPropertyEditor.GetValue: String;
var
  B: TBitmap;
begin
  B := TscGPImageCollectionItem(GetComponent(0)).Bitmap;
  if not B.Empty
  then
  begin
    if B.PixelFormat = pf32Bit then
      Result := 'TBitmap[32Bit]' + '(' + IntToStr(B.Width) + 'x' + IntToStr(B.Height) + ')'
    else
      Result := 'TBitmap';
  end
  else
    Result := '(None)';
end;


procedure TscGPCharPropertyEditor.Edit;
var
  C: TPersistent;
  I: Integer;
begin
  C := GetComponent(0);
  if C is TscGPCharGlyphOptions then
  begin
    I := TscGPCharGlyphOptions(C).Index;
    ExecuteGPCharEditor(I);
    TscGPCharGlyphOptions(C).Index := I;
  end;
  Designer.Modified;
end;

function TscGPCharPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TscGPCharImagePropertyEditor.GetValue: String;
var
  C: TPersistent;
  I: Integer;
begin
  C := GetComponent(0);
  if C is TscGPCharImage then
  begin
    I := TscGPCharImage(C).ImageIndex;
    Result := IntToStr(I);
  end;
end;

procedure TscGPCharImagePropertyEditor.Edit;
var
  C: TPersistent;
  I: Integer;
begin
  C := GetComponent(0);
  if C is TscGPCharImage then
  begin
    I := TscGPCharImage(C).ImageIndex;
    ExecuteGPCharEditor(I);
    TscGPCharImage(C).ImageIndex := I;
  end;
  Designer.Modified;
end;

function TscGPCharImagePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TscGPCharPropertyEditor.GetValue: String;
var
  C: TPersistent;
  I: Integer;
begin
  C := GetComponent(0);
  if C is TscGPCharGlyphOptions then
  begin
    I := TscGPCharGlyphOptions(C).Index;
    Result := IntToStr(I);
  end;
end;
procedure TscGPPngPropertyEditor.Edit;
begin
  if PropCount <> 1 then Exit;
  ExecutePngEditor(TscGPImageCollectionItem(GetComponent(0)).PngImage);
  TscGPImageCollectionItem(GetComponent(0)).ImageChanged;
  Designer.Modified;
end;

function TscGPPngPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TscGPPngPropertyEditor.GetValue: String;
var
  PngImage: TPngImage;
begin
  PngImage := TscGPImageCollectionItem(GetComponent(0)).PngImage;
  if not PngImage.Empty
  then
    Result := 'TPngImage' + '(' + IntToStr(PngImage.Width) + 'x' + IntToStr(PngImage.Height) + ')'
  else
    Result := '(None)';
end;

procedure TscPngPropertyEditor.Edit;
begin
  if PropCount <> 1 then Exit;
  ExecutePngEditor(TscGPImage(GetComponent(0)).PngImage);
  TscGPImage(GetComponent(0)).ImageChanged;
  Designer.Modified;
end;

function TscPngPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TscPngPropertyEditor.GetValue: String;
var
  PngImage: TPngImage;
begin
  PngImage := TscGPImage(GetComponent(0)).PngImage;
  if not PngImage.Empty
  then
    Result := 'TPngImage' + '(' + IntToStr(PngImage.Width) + 'x' + IntToStr(PngImage.Height) + ')'
  else
    Result := '(None)';
end;

procedure TscBitmapPropertyEditor.Edit;
begin
  if PropCount <> 1 then Exit;
  ExecutePngToBitmapEditor(TscImageCollectionItem(GetComponent(0)).Bitmap, True);
  TscImageCollectionItem(GetComponent(0)).CheckBitmapOptions;
  Designer.Modified;
end;

function TscBitmapPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TscBitmapPropertyEditor.GetValue: String;
var
  B: TBitmap;
begin
  B := TscImageCollectionItem(GetComponent(0)).Bitmap;
  if not B.Empty
  then
  begin
    if B.PixelFormat = pf32Bit then
      Result := 'TBitmap(32Bit)'
    else
      Result := 'TBitmap';
  end
  else
    Result := '(None)';
end;

procedure Register;
begin
  RegisterClass(TscTabSheet);
  RegisterClass(TscGPPageControlPage);
  RegisterClass(TscGPVertPageControlPage);
  RegisterClass(TscGPToolPagerPage);
  RegisterClass(TscAppPagerPage);
  RegisterClass(TscToolPagerPage);
  RegisterClass(TscPageViewerPage);
  RegisterClass(TscGPPageViewerPage);
  RegisterClass(TscAdvancedPagerPage);

  RegisterComponentEditor(TscGPPageControl, TscGPPageControlEditor);
  RegisterComponentEditor(TscGPVertPageControl, TscGPVertPageControlEditor);
  RegisterComponentEditor(TscGPToolPager, TscGPToolPagerEditor);
  RegisterComponentEditor(TscPageControl, TscPageControlEditor);
  RegisterComponentEditor(TscTabSheet, TscPageControlEditor);
  RegisterComponentEditor(TscAppPager, TscAppPagerEditor);
  RegisterComponentEditor(TscToolPager, TscToolPagerEditor);
  RegisterComponentEditor(TscAdvancedPager, TscAdvancedPagerEditor);
  RegisterComponentEditor(TscPageViewer, TscPageViewerEditor);
  RegisterComponentEditor(TscGPPageViewer, TscGPPageViewerEditor);
  RegisterComponentEditor(TscPageViewerPage, TscPageViewerEditor);
  RegisterComponentEditor(TscGPPageViewerPage, TscGPPageViewerEditor);
  RegisterComponentEditor(TscSwitch, TscSwitchEditor);
  RegisterComponentEditor(TscToggleSwitch, TscSwitchEditor);
  RegisterComponentEditor(TscGPSwitch, TscSwitchEditor);
  RegisterComponentEditor(TscGPToggleSwitch, TscSwitchEditor);
  RegisterComponentEditor(TscAdvancedListBox, TscAdvancedListEditor);
  RegisterComponentEditor(TscGPListBox, TscAdvancedListEditor);
  RegisterComponentEditor(TscHorzListBox, TscAdvancedListEditor);
  RegisterComponentEditor(TscGridView, TscAdvancedListEditor);
  RegisterComponentEditor(TscGallery, TscAdvancedListEditor);
  RewriteBitmapPublishedProperty(TscImageCollectionItem, 'Bitmap');

  GPRewriteBitmapPublishedProperty(TscGPImageCollectionItem, 'Bitmap');
  GPRewritePngPublishedProperty(TscGPImageCollectionItem, 'PngImage');
  GPRewriteCharPublishedProperty(TscGPCharGlyphOptions, 'Index');
  GPRewriteCharImagePublishedProperty(TscGPCharImage, 'ImageIndex');

  RewritePngPublishedProperty(TscGPImage, 'PngImage');
  RegisterPropertyEditor(TypeInfo(string), TscColumn, 'FieldName', TscColumnDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TscDBLookUpComboBox, 'KeyField', TscDBLookUpComboBoxFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TscDBLookUpComboBox, 'ListField', TscDBLookUpComboBoxFieldProperty);
  RegisterComponents('StyleControls',
    [TscLabel, TscButton, TscCheckBox, TscRadioButton,
    TscPanel, TscGroupBox, TscListBox, TscComboBox, TscComboBoxEx, TscCheckListBox, TscPageControl, TscTreeView, TscListView,
    TscEdit, TscNumericEdit, TscTrackEdit, TscSpinEdit, TscTimeEdit, TscDateEdit, TscMonthCalendar,
    TscMemo, TscRadioGroup,
    TscCheckGroup, TscImageCollection, TscPasswordEdit,
    TscColorListBox, TscColorBox, TscFontListBox, TscFontComboBox, TscFontSizeComboBox,
    TscCalcEdit,
    TscColorButton, TscScrollBox, TscTrackBar, TscProgressBar, TscStatusBar, TscToolBar,
    TscControlBar, TscCoolBar,
    TscBalloonHint, TscHint, TscWebBrowser, TscRichEdit, TscDrawGrid, TscStringGrid, TscImage, TscScrollBar]);
  RegisterComponents('StyleControls Shell',
    [TscShellTreeView, TscShellListView, TscShellComboBox, TscFilterComboBox]);
  RegisterComponents('StyleControls Advanced',
    [TscAdvancedListBox, TscAdvancedComboBox,
     TscHorzListBox, TscLinkBar, TscGridView,
     TscGridViewComboBox, TscGallery, TscAdvancedComboEdit, TscButtonsBar, TscFrameBar,
     TscGalleryMenu, TscAppPager, TscScrollPanel, TscToolPager, TscToolGroupPanel,
     TscAdvancedPager, TscAdvancedTabControl,
     TscPageViewer,
     TscSplitView, TscModernSplitView, TscSwitch, TscToggleSwitch,
     TscRelativePanel, TscGridPanel,
     TscActivityIndicator,
     TscHtmlLabel, TscExPanel, TscListGroupPanel]);
   RegisterComponents('StyleControls Colors',
     [TscCustomColorGrid, TscColorGrid, TscLColorPicker, TscHSColorPicker]);
  RegisterComponents('StyleControls Tools',
    [TscStyleManager, TscStyledForm]);
  RegisterComponents('StyleControls DB',
    [TscDBText, TscDBEdit, TscDBMemo, TscDBComboBox, TscDBListBox,
     TscDBCheckBox, TscDBRadioGroup, TscDBLookUpListBox, TscDBLookUpComboBox,
     TscDBNumericEdit, TscDBSpinEdit, TscDBCalcEdit,
     TscDBDateEdit, TscDBTimeEdit, TscDBPasswordEdit, TscDBProgressBar,
     TscDBTrackBar, TscDBRichEdit, TscDBImage, TscDBNavigator, TscDBGrid,
     TscDBAdvancedComboEdit, TscDBAdvancedComboBox, TscDBAdvancedListBox,
     TscDBToggleSwitch]);

  RegisterComponents('StyleControls GP',
    [TscGPVirtualImageList, TscGPImageCollection,
     TscGPLabel, TscGPButton, TscGPCheckBox, TscGPRadioButton,
     TscGPActivityBar,
     TscGPSwitch, TscGPToggleSwitch,
     TscGPTrackBar, TscGPCircledProgressBar,
     TscGPProgressBar, TscGPPanel,
     TscGPImage, TscGPCharImage,
     TscGPMeter, TscGPMeter120, TscGPMeter90,
     TscGPDial, TscGPGearDial, TscGPClock,
     TscGPHVMeter, TscGPSlider,
     TscGPListBox, TscGPComboBox, TscGPEdit, TscGPComboEdit,
     TscGPNumericEdit, TscGPSpinEdit, TscGPTimeEdit,
     TscGPPasswordEdit, TscGPMonthCalendar, TscGPDateEdit,
     TscGPMemo, TscGPGroupBox,
     TscGPGlyphButton, TscGPCharGlyphButton,
     TscGPGearActivityIndicator, TscGPScrollPanel,
     TscGPPageControl, TscGPTabControl,
     TscGPVertPageControl, TscGPVertTabControl,
     TscGPToolPager, TscGPToolGroupPanel, TscGPSizeBox,
     TscGPPageViewer]);

  RegisterComponents('StyleControls GP DB',
    [TscGPDBCheckBox, TscGPDBTrackBar, TscGPDBCircledProgressBar,
     TscGPDBProgressBar, TscGPDBMeter, TscGPDBMeter120, TscGPDBHVMeter,
     TscGPDBSlider, TscGPDBDial, TscGPDBGearDial,
     TscGPDBEdit, TscGPDBComboEdit, TscGPDBListBox, TscGPDBComboBox,
     TscGPDBNumericEdit, TscGPDBSpinEdit, TscGPDBTimeEdit,
     TscGPDBPasswordEdit, TscGPDBDateEdit, TscGPDBMemo, TscGPDBText,
     TscGPDBToggleSwitch]);

  RegisterComponents('StyleControls Dialogs',
    [TscSelectPathDialog, TscOpenDialog, TscSaveDialog,
    TscOpenPictureDialog, TscSavePictureDialog,
    TscColorDialog, TscFontDialog, TscPrintDialog]);
end;

end.
