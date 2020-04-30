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


unit scWebBrowser;

{$I scdefine.inc}
{$R-}

interface

{$IFDEF VER300_UP}
 Uses SHDocVw;
 type
   TscWebBrowser = class(SHDocVw.TWebBrowser);
{$ELSE}

uses
  WinApi.Windows, WinApi.Messages, WinApi.ActiveX,
  Vcl.OleServer, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, SHDocVw, MSHTML;

type

  PDOCHOSTUIINFO = ^TDOCHOSTUIINFO;
  TDOCHOSTUIINFO = packed record
    cbSize: ULONG;
    dwFlags: DWORD;
    dwDoubleClick: DWORD;
    pchHostCss: PWChar;
    pchHostNS: PWChar;
  end;

  TEnhancedWebBrowserUI = class(TPersistent)
  private
    FlatScrollBar: boolean;
    IE3DBorder: boolean;
    RightClickMenu: boolean;
    ScrollBar: boolean;
  public
    constructor Create;
  published
    property EnableScrollBars: boolean read ScrollBar write ScrollBar;
    property EnableFlatScrollBars: boolean read FlatScrollBar write
      FlatScrollBar;
    property EnableContextMenu: boolean read RightClickMenu write
      RightClickMenu;
    property Enable3DBorder: boolean read IE3DBorder write IE3DBorder;
  end;

  IDocHostUIHandler = interface(IUnknown)
    ['{bd3f23c0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const pcmdtReserved: IUnknown; const pdispReserved: IDispatch):
      HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject:
      IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame:
      IOleInPlaceFrame;                                              
      const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow;
      const FrameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup:
      PGUID;
      const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD):
      HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject;
      out ppDORet: IDataObject): HRESULT; stdcall;
  end;

  TScrollWinControl = class(TWinControl)
  protected
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TscWebBrowser = class(SHDocVw.TWebBrowser, IDocHostUIHandler)
  private
    FFrameDoc: Boolean;
    FVScrollWnd, FHScrollWnd, FSizeGrip: TScrollWinControl;
    FVScroll, FHScroll: TScrollBar;
    UIProperties: TEnhancedWebBrowserUI;
    procedure CMVisibleChanged(var MSg: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure InvokeEvent(DispID: TDispID; var Params: TDispParams); override;

    procedure DoBeforeNavigate2(Sender: TObject;
      const pDisp: IDispatch;
      const  URL, Flags, TargetFrameName, PostData,
       Headers: OleVariant; var Cancel: WordBool);

    procedure DoCommandStateChange(Sender: TObject;
      Command: Integer; Enable: WordBool);

    procedure DoNavigateComplete2(Sender: TObject;
       const pDisp: IDispatch;
       const URL: OleVariant);

    procedure DoDocumentComplete(Sender: TObject;
       const pDisp: IDispatch;
       const URL: OleVariant);

    procedure DoProgressChange(Sender: TObject; Progress,
       ProgressMax: Integer);

    procedure DoVScrollScroll(Sender: TObject);
    procedure DoHScrollScroll(Sender: TObject);
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure WMSIZE(var Message: TWMSIZE); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateScrollbars;
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const pcmdtReserved: IUnknown; const pdispReserved: IDispatch):
      HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject:
      IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame:
      IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow;
      const fRameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup:
      PGUID;
      const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD):
      HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject;
      out ppDORet: IDataObject): HRESULT; stdcall;
    property UISettings: TEnhancedWebBrowserUI read UIProperties write UIProperties;
  end;

const
  DOCHOSTUIFLAG_SCROLL_NO = $00000008;
  DOCHOSTUIFLAG_FLAT_SCROLLBAR = $00000080;
  DOCHOSTUIFLAG_NO3DBORDER = $0004;
  DOCHOSTUIFLAG_THEME = $00040000;

 {$ENDIF}

implementation

{$IFNDEF VER300_UP}

  Uses System.SysUtils, scDrawUtils;

constructor TscWebBrowser.Create(AOwner: TComponent);
begin
  FVScrollWnd := nil;
  FHScrollWnd := nil;
  inherited;
  FFrameDoc := False;

  UIProperties := TEnhancedWebBrowserUI.Create;

  if not (csDesigning in ComponentState) then
  begin
    FVScrollWnd := TScrollWinControl.Create(Self);
    FVScrollWnd.Visible := True;

    FVScroll := TScrollBar.Create(Self);
    FVScroll.Kind := sbVertical;
    FVScroll.Parent := FVScrollWnd;
    FVScroll.Visible := True;
    FVScroll.Align := alClient;
    FVScroll.ParentCtl3D := False;
    FVScroll.Ctl3D := False;
    FVScroll.OnChange := DoVScrollScroll;
    FVScroll.Enabled := False;

    FHScrollWnd := TScrollWinControl.Create(Self);
    FHScrollWnd.Visible := False;

    FHScroll := TScrollBar.Create(Self);
    FHScroll.Parent := FHScrollWnd;
    FHScroll.Visible := True;
    FHScroll.Align := alClient;
    FHScroll.ParentCtl3D := False;
    FHScroll.Ctl3D := False;
    FHScroll.OnChange := DoHScrollScroll;

    FSizeGrip := TScrollWinControl.Create(Self);
    FSizeGrip.Visible := False;
  end;

  Width := 100;
  Height := 100;
end;

destructor TscWebBrowser.Destroy;
begin
  UIProperties.Free;
  inherited;
end;

procedure TscWebBrowser.InvokeEvent(DispID: TDispID; var Params: TDispParams);
var
  FParamCount: Integer;
  FParamsArray: Array of OleVariant;
  FIndex: Integer;
begin
  inherited;
  FParamCount := Params.cArgs;
  SetLength(FParamsArray, FParamCount);
  for FIndex := Low(FParamsArray) to High(FParamsArray) do
    FParamsArray[High(FParamsArray) - FIndex] :=
      OleVariant(TDispParams(Params).rgvarg^[FIndex]);
  case DispID of
    252:
      DoNavigateComplete2(Self, FParamsArray[0], FParamsArray[1]);
    259:
      DoDocumentComplete(Self, FParamsArray[0], FParamsArray[1]);
    250:
      DoBeforeNavigate2(Self, FParamsArray[0], FParamsArray[1],
        FParamsArray[2], FParamsArray[3],
        FParamsArray[4], FParamsArray[5],
        WordBool((TVarData(FParamsArray[6]).VPointer)^));
    105:
      DoCommandStateChange(Self, FParamsArray[0],  FParamsArray[1]);
    108:
      DoProgressChange(Self, FParamsArray[0], FParamsArray[1]);
  end;
  SetLength(FParamsArray, 0);
end;

procedure TscWebBrowser.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 10 then
  begin
    KillTimer(Handle, 10);
    UpdateScrollBars;
    if FHScrollWnd.Visible then
      FHScroll.Repaint;
    if FVScrollWnd.Visible then
      FVScroll.Repaint;
  end;
end;

procedure TscWebBrowser.WMSIZE(var Message: TWMSIZE);
begin
  if Document <> nil then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

  inherited;
  UpdateScrollbars;

  if Document <> nil then
  begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Handle, nil, 0,
      RDW_INVALIDATE + RDW_ALLCHILDREN + RDW_UPDATENOW);
  end;
end;

function TscWebBrowser.GetOptionKeyPath(var pchKey: POLESTR;
  const dw: DWORD): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.TranslateAccelerator(const lpMsg: PMSG;
  const pguidCmdGroup: PGUID; const nCmdID: DWORD): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.TranslateUrl(const dwTranslate: DWORD;
  const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.EnableModeless(const fEnable: BOOL): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.FilterDataObject(const pDO: IDataObject;
  out ppDORet: IDataObject): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.GetDropTarget(const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.GetExternal(out ppDispatch: IDispatch): HRESULT;
begin
  result := S_OK;
end;

function TscWebBrowser.UpdateUI: HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.HideUI: HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.OnDocWindowActivate(
  const fActivate: BOOL): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.OnFrameWindowActivate(
  const fActivate: BOOL): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.ResizeBorder(const prcBorder: PRECT;
  const pUIWindow: IOleInPlaceUIWindow; const fRameWindow: BOOL): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.ShowUI(const dwID: DWORD;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HRESULT;
begin
  result := S_FALSE;
end;

function TscWebBrowser.ShowContextMenu(const dwID: DWORD;
  const ppt: PPOINT; const pcmdtReserved: IUnknown;
  const pdispReserved: IDispatch): HRESULT;
begin
  if UIProperties.EnableContextMenu then
    result := S_FALSE
  else
    result := S_OK;
end;

procedure TscWebBrowser.SetParent(AParent: TWinControl);
begin
  inherited;
  if not (csDesigning in ComponentState) and not (csDestroying in ComponentState) then
  begin
    FVScrollWnd.Parent := AParent;
    FHScrollWnd.Parent := AParent;
    FSizeGrip.Parent := AParent;
    UpdateScrollBars;
  end;
end;

procedure TscWebBrowser.UpdateScrollBars;
var
  R: TRect;
  OldVisible, VisibleChanged: Boolean;
  HPageSize, VPageSize, CW, SW, SH: Integer;
  FXHTHUMB, FYHTHUMB: Integer;
begin
  if (csDesigning in ComponentState) then Exit;
  if (FVScrollWnd = nil) or (FHScrollWnd = nil) then Exit;
  if FFrameDoc then FVScrollWnd.Visible := False else FVScrollWnd.Visible := True;
  FXHTHUMB := GetSystemMetrics(SM_CXHTHUMB);
  FYHTHUMB := GetSystemMetrics(SM_CYVTHUMB);

  if (Document <> nil) and (IHtmldocument2(Document).Body <> nil)
  then
   begin
     CW := Variant(Document).documentElement.ClientWidth;
     if CW > 0
     then
       begin
         SW := Variant(Document).DocumentElement.scrollWidth;
         if (FHScroll.Max <> SW) and (SW >= FHScroll.PageSize) and (SW >= FHScroll.Min)
         then
           FHScroll.Max := SW;
         SH := Variant(Document).DocumentElement.scrollHeight;
         if (FVScroll.Max <> SH) and (SH >= FVScroll.PageSize) and
            (SH >= FVScroll.Min) then
          FVScroll.Max := SH;
       end
     else
       begin
         SH := IHtmldocument2(Document).Body.getAttribute('ScrollHeight', 0);
         if (FVScroll.Max <> SH) and (SH >= FVScroll.PageSize) and
            (SH >= FVScroll.Min) then
          FVScroll.Max := SH;
         SW := IHtmldocument2(Document).Body.getAttribute('ScrollWidth', 0);
         if (FHScroll.Max <> SW) and (SW >= FHScroll.PageSize) and (SW >= FHScroll.Min)
         then
           FHScroll.Max := SW;
       end;

     if (FHScroll.Max > Self.Width - FXHTHUMB) and
        (FHScroll.Max > 0) and (FHScroll.Max <> Self.Width)
     then
       VPageSize := Self.Height - FYHTHUMB
     else
       VPageSize := Self.Height;

     FVScroll.PageSize := VPageSize;
     FVScroll.SetParams(FVScroll.Position, 0, FVScroll.Max);
     FVScroll.LargeChange := TScrollBarInc(FVScroll.PageSize);

     FVScroll.Enabled := (VPageSize < FVScroll.Max) and
                         (FVScroll.PageSize > 0) and (FVScroll.Max > 0) and
                         (FVScroll.Max <> Self.Height);

     HPageSize := Self.Width - FXHTHUMB;
     FHScroll.PageSize := HPageSize;
     FHScroll.SetParams(FHScroll.Position, 0, FHScroll.Max);
     FHScroll.LargeChange := TScrollBarInc(FHScroll.PageSize);

     OldVisible := FHScrollWnd.Visible;
     FHScrollWnd.Visible := (HPageSize < FHScroll.Max) and
                            (FHScroll.PageSize < FHScroll.Max) and (FHScroll.Max > 0) and
                            (FHScroll.Max <> Self.Width);

     VisibleChanged := FHScrollWnd.Visible <> OldVisible;

     if VisibleChanged and (FHScrollWnd.Visible or FVScrollWnd.Visible)
     then
     begin
       if FHScrollWnd.Visible then
         FHScrollWnd.BringToFront;
       if FVScrollWnd.Visible then
         FVScrollWnd.BringToFront;
       SetTimer(Handle, 10, 50, nil);
     end;
   end;

  if FVScrollWnd.Visible then
  begin
    R := BoundsRect;
    R.Left := R.Right - FXHTHUMB;
    if FHScrollWnd.Visible then
      R.Bottom := R.Bottom - FYHTHUMB;
    FVScrollWnd.BoundsRect := R;
  end;

  if FHScrollWnd.Visible then
  begin
    R := BoundsRect;
    R.Top := R.Bottom - FYHTHUMB;
    if FVScrollWnd.Visible then
      R.Right := R.Right - FXHTHUMB;
    FHScrollWnd.BoundsRect := R;
  end;

  OldVisible := FSizeGrip.Visible;
  FSizeGrip.Visible := FHScrollWnd.Visible and FVScrollWnd.Visible;

  if FSizeGrip.Visible
  then
    begin
      R := BoundsRect;
      R.Left := R.Right - FXHTHUMB;
      R.Top := R.Bottom - FYHTHUMB;
      FSizeGrip.BoundsRect := R;
      if not OldVisible then FSizeGrip.BringToFront;
    end;
end;

procedure TscWebBrowser.DoProgressChange(Sender: TObject; Progress,
       ProgressMax: Integer);
begin
  if (csDesigning in ComponentState) then Exit;
  UpdateScrollbars;
end;

procedure TscWebBrowser.DoDocumentComplete;
begin
  if (csDesigning in ComponentState) then Exit;
  UpdateScrollbars;
end;

procedure TscWebBrowser.DoNavigateComplete2;
var
  I: Integer;
  iDoc: IHtmldocument2;
  iDisp : IDispatch;
  iElement : IHTMLElement;
  S: String;
begin
  if (csDesigning in ComponentState) then Exit;
  FFrameDoc := False;
  iDoc := IHtmldocument2(Document);
  for i := 0 to iDoc.All.Get_length - 1 do
  begin
    iDisp := iDoc.Get_all.item(i, 0);
    iDisp.QueryInterface(IHTMLElement, iElement);
    if Assigned(iElement) then
    begin
      S := iElement.Get_tagName;
      if UpperCase(S) = 'FRAMESET' then
      begin
        FFrameDoc := True;
        Break;
      end;
    end;
  end;
  UpdateScrollbars;
end;

procedure TscWebBrowser.DoVScrollScroll;
begin
  if (csDesigning in ComponentState) then Exit;
  if Document <> nil
  then
    IHTMLWindow2(IHTMLDocument2(Document).ParentWindow).Scroll(FHScroll.Position, FVScroll.Position);
end;

procedure TscWebBrowser.DoHScrollScroll;
begin
  if (csDesigning in ComponentState) then Exit;
  if Document <> nil
  then
    IHTMLWindow2(IHTMLDocument2(Document).ParentWindow).Scroll(FHScroll.Position, FVScroll.Position);
end;

procedure TscWebBrowser.CMVisibleChanged(var MSg: TMessage);
begin
  inherited ;
  if (csDesigning in ComponentState) then Exit;
  if not Visible then
  begin
    FVScrollWnd.Visible := False;
    FHScrollWnd.Visible := False;
    FSizeGrip.Visible := False;
  end;
end;

procedure TscWebBrowser.Loaded;
begin
  inherited;
  UpdateScrollbars;
end;

procedure TscWebBrowser.DoCommandStateChange;
begin
  if (csDesigning in ComponentState) then Exit;
  if (Document <> nil) and (IHtmldocument2(Document).Body <> nil)
  then
  begin
    if Variant(Document).DocumentElement.scrollTop = 0
    then
      FVScroll.Position := IHtmldocument2(Document).Body.getAttribute('ScrollTop', 0)
    else
      FVScroll.Position := Variant(Document).DocumentElement.scrollTop;
    if Variant(Document).DocumentElement.scrollLeft = 0
    then
      FHScroll.Position := IHtmldocument2(Document).Body.getAttribute('ScrollLeft', 0)
    else
      FHScroll.Position := Variant(Document).DocumentElement.scrollLeft
  end;
  UpdateScrollbars;
end;

procedure TscWebBrowser.DoBeforeNavigate2;
begin
  if (csDesigning in ComponentState) then Exit;
  UpdateScrollbars;
end;

function TscWebBrowser.GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT;
begin
  if (csDesigning in ComponentState) then
  begin
    Result := S_OK;
    Exit;
  end;

  pInfo.cbSize := SizeOf(pInfo);
  pInfo.dwFlags := 0;
  if (not UIProperties.EnableScrollBars) then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_SCROLL_NO;
  if UIProperties.EnableFlatScrollBars then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_FLAT_SCROLLBAR;
  if not UIProperties.Enable3DBorder then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_NO3DBORDER;

  pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_THEME;

  Result := S_OK;

  UpdateScrollBars;
end;

constructor TEnhancedWebBrowserUI.Create;
begin
  ScrollBar := True;
  FlatScrollBar := False;
  IE3DBorder := False;
  RightClickMenu := True;
end;


constructor TScrollWinControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TScrollWinControl.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  FCanvas: TCanvas;
begin
  FCanvas := TCanvas.Create;
  FCanvas.Handle := Msg.DC;
  FCanvas.Brush.Color := GetStyleColor(clBtnFace);
  FCanvas.FillRect(ClientRect);
  FCanvas.Handle := 0;
  FCanvas.Free;
end;

{$ENDIF}

end.
