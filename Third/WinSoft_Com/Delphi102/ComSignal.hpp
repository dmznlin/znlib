// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ComSignal.pas' rev: 32.00 (Windows)

#ifndef ComsignalHPP
#define ComsignalHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <ComPort.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Comsignal
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TComSignal;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSignal : unsigned char { siNone, siBreak, siRxChar, siTxChar, siCTS, siDSR, siEvent1, siEvent2, siLineError, siPrinterError, siRing, siRLSD, siRx80PercFull, siRxFlag, siTxEmpty };

class PASCALIMPLEMENTATION TComSignal : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Uitypes::TColor FColorOff;
	System::Uitypes::TColor FColorOn;
	Comport::TComPort* FComPort;
	Vcl::Controls::TControl* FControl;
	int FDelay;
	TSignal FSignal;
	bool FSignalValue;
	Vcl::Extctrls::TTimer* FTimer;
	bool FWriting;
	Comport::TOpenCloseEvent FAfterClose;
	Comport::TOpenCloseEvent FAfterOpen;
	Comport::TReadWriteEvent FAfterWrite;
	Comport::TReadWriteEvent FBeforeWrite;
	System::Classes::TNotifyEvent FOnNotifySignal;
	Comport::TLineErrorEvent FOnLineError;
	System::Classes::TNotifyEvent FOnModemSignal;
	System::Classes::TNotifyEvent FOnSignal;
	System::UnicodeString __fastcall GetAbout(void);
	void __fastcall SetAbout(const System::UnicodeString Value);
	void __fastcall SetColorOff(System::Uitypes::TColor Value);
	void __fastcall SetComPort(Comport::TComPort* Value);
	void __fastcall SetControl(Vcl::Controls::TControl* Value);
	void __fastcall SetDelay(int Value);
	void __fastcall SetSignalValue(bool Value);
	
protected:
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall UpdateColor(void);
	void __fastcall UpdateSignalValue(void);
	void __fastcall RetrieveEvents(void);
	void __fastcall RestoreEvents(void);
	void __fastcall StartTimer(void);
	void __fastcall AfterClose(Comport::TCustomComPort* ComPort);
	void __fastcall AfterOpen(Comport::TCustomComPort* ComPort);
	void __fastcall AfterWrite(System::TObject* Sender, void * Buffer, int Length, bool WaitOnCompletion);
	void __fastcall BeforeWrite(System::TObject* Sender, void * Buffer, int Length, bool WaitOnCompletion);
	void __fastcall OnNotifySignal(System::TObject* Sender);
	void __fastcall OnLineError(System::TObject* Sender, Comport::TLineErrors LineErrors);
	void __fastcall OnModemSignal(System::TObject* Sender);
	void __fastcall OnTimer(System::TObject* Sender);
	
public:
	__fastcall virtual TComSignal(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TComSignal(void);
	
__published:
	__property System::UnicodeString About = {read=GetAbout, write=SetAbout, stored=false};
	__property System::Uitypes::TColor ColorOff = {read=FColorOff, write=SetColorOff, default=32768};
	__property System::Uitypes::TColor ColorOn = {read=FColorOn, write=FColorOn, default=65280};
	__property Comport::TComPort* ComPort = {read=FComPort, write=SetComPort};
	__property Vcl::Controls::TControl* Control = {read=FControl, write=SetControl};
	__property int Delay = {read=FDelay, write=SetDelay, nodefault};
	__property TSignal Signal = {read=FSignal, write=FSignal, nodefault};
	__property bool SignalValue = {read=FSignalValue, stored=false, nodefault};
	__property System::Classes::TNotifyEvent OnSignal = {read=FOnSignal, write=FOnSignal};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Comsignal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_COMSIGNAL)
using namespace Comsignal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ComsignalHPP
