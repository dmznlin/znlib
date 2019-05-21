// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ComPort.pas' rev: 32.00 (Windows)

#ifndef ComportHPP
#define ComportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Messages.hpp>

//-- user supplied -----------------------------------------------------------

namespace Comport
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EComError;
class DELPHICLASS TComThread;
class DELPHICLASS TFlowControl;
class DELPHICLASS TCharacters;
class DELPHICLASS TBufferSizes;
class DELPHICLASS TTimeouts;
class DELPHICLASS TCustomComPort;
class DELPHICLASS TComPort;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EComError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	int ErrorCode;
	__fastcall EComError(int ErrorCode, const System::UnicodeString Msg);
public:
	/* Exception.CreateFmt */ inline __fastcall EComError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EComError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EComError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EComError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EComError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EComError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EComError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EComError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EComError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EComError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EComError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EComError(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TComThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TCustomComPort* FComPort;
	
public:
	__fastcall TComThread(TCustomComPort* ComPort);
	virtual void __fastcall Execute(void);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TComThread(void) { }
	
};


enum DECLSPEC_DENUM TBaudRate : unsigned char { brDefault, br50, br75, br110, br150, br300, br600, br1200, br1800, br2000, br2400, br3600, br4800, br7200, br9600, br10400, br14400, br15625, br19200, br28800, br38400, br56000, br57600, br115200, br128000, br256000, brCustom };

enum DECLSPEC_DENUM TParity : unsigned char { paDefault, paNone, paOdd, paEven, paMark, paSpace };

enum DECLSPEC_DENUM TStopBits : unsigned char { sbDefault, sb1, sb1_5, sb2 };

enum DECLSPEC_DENUM TDataBits : unsigned char { dbDefault, db4, db5, db6, db7, db8, db9 };

enum DECLSPEC_DENUM TOption : unsigned char { opCheckParity, opOutputCTSFlow, opOutputDSRFlow, opDSRSensitivity, opTXContinueOnXOff, opUseErrorChar, opDiscardNullBytes, opAbortOnError };

typedef System::Set<TOption, TOption::opCheckParity, TOption::opAbortOnError> TOptions;

enum DECLSPEC_DENUM TModemStatusValue : unsigned char { msCTS, msDSR, msRing, msRLSD };

typedef System::Set<TModemStatusValue, TModemStatusValue::msCTS, TModemStatusValue::msRLSD> TModemStatus;

enum DECLSPEC_DENUM TLineError : unsigned char { leBreak, leDeviceNotSelected, leFrame, leIO, leMode, leOutOfPaper, leOverrun, leDeviceTimeOut, leRxOverflow, leParity, leTxFull };

typedef System::Set<TLineError, TLineError::leBreak, TLineError::leTxFull> TLineErrors;

enum DECLSPEC_DENUM TDTRControl : unsigned char { dcDefault, dcDisable, dcEnable, dcHandshake };

enum DECLSPEC_DENUM TRTSControl : unsigned char { rcDefault, rcDisable, rcEnable, rcHandshake, rcToggle };

enum DECLSPEC_DENUM TXOnXOffControl : unsigned char { xcDefault, xcDisable, xcInput, xcOutput, xcInputOutput };

enum DECLSPEC_DENUM TPriorityClass : unsigned char { pcDefault, pcIdle, pcNormal, pcHigh, pcRealTime };

typedef void __fastcall (__closure *TOpenCloseEvent)(TCustomComPort* ComPort);

typedef void __fastcall (__closure *TReadWriteEvent)(System::TObject* Sender, void * Buffer, int Length, bool WaitOnCompletion);

enum DECLSPEC_DENUM TComAction : unsigned char { caFail, caAbort };

typedef void __fastcall (__closure *TComErrorEvent)(TCustomComPort* ComPort, EComError* E, TComAction &Action);

typedef void __fastcall (__closure *TLineErrorEvent)(System::TObject* Sender, TLineErrors LineErrors);

typedef void __fastcall (__closure *TDeviceArrivalEvent)(System::TObject* Sender, const System::UnicodeString DeviceName);

typedef void __fastcall (__closure *TDeviceRemovedEvent)(System::TObject* Sender, const System::UnicodeString DeviceName);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFlowControl : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TCustomComPort* FComPort;
	TDTRControl FDTRControl;
	TRTSControl FRTSControl;
	TXOnXOffControl FXOnXOffControl;
	System::Word FXOnLimit;
	System::Word FXOffLimit;
	void __fastcall SetDTRControl(TDTRControl Value);
	void __fastcall SetRTSControl(TRTSControl Value);
	void __fastcall SetXOnXOffControl(TXOnXOffControl Value);
	void __fastcall SetXOnLimit(System::Word Value);
	void __fastcall SetXOffLimit(System::Word Value);
	
public:
	__fastcall TFlowControl(TCustomComPort* ComPort);
	
__published:
	__property TDTRControl DTR = {read=FDTRControl, write=SetDTRControl, default=0};
	__property TRTSControl RTS = {read=FRTSControl, write=SetRTSControl, default=0};
	__property TXOnXOffControl XOnXOff = {read=FXOnXOffControl, write=SetXOnXOffControl, default=0};
	__property System::Word XOnLimit = {read=FXOnLimit, write=SetXOnLimit, default=0};
	__property System::Word XOffLimit = {read=FXOffLimit, write=SetXOffLimit, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TFlowControl(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCharacters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TCustomComPort* FComPort;
	System::WideChar FXOn;
	System::WideChar FXOff;
	System::WideChar FError;
	System::WideChar FEof;
	System::WideChar FEvent;
	void __fastcall SetXOn(System::WideChar Value);
	void __fastcall SetXOff(System::WideChar Value);
	void __fastcall SetError(System::WideChar Value);
	void __fastcall SetEof(System::WideChar Value);
	void __fastcall SetEvent(System::WideChar Value);
	
public:
	__fastcall TCharacters(TCustomComPort* ComPort);
	
__published:
	__property System::WideChar XOn = {read=FXOn, write=SetXOn, default=17};
	__property System::WideChar XOff = {read=FXOff, write=SetXOff, default=19};
	__property System::WideChar Error = {read=FError, write=SetError, default=0};
	__property System::WideChar Eof = {read=FEof, write=SetEof, default=0};
	__property System::WideChar Event = {read=FEvent, write=SetEvent, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCharacters(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBufferSizes : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TCustomComPort* FComPort;
	int FInput;
	int FOutput;
	void __fastcall SetInput(int Value);
	void __fastcall SetOutput(int Value);
	
public:
	__fastcall TBufferSizes(TCustomComPort* ComPort);
	
__published:
	__property int Input = {read=FInput, write=SetInput, default=4096};
	__property int Output = {read=FOutput, write=SetOutput, default=2048};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TBufferSizes(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTimeouts : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TCustomComPort* FComPort;
	int FReadInterval;
	int FReadMultiplier;
	int FReadConstant;
	int FWriteMultiplier;
	int FWriteConstant;
	void __fastcall SetReadInterval(int Value);
	void __fastcall SetReadMultiplier(int Value);
	void __fastcall SetReadConstant(int Value);
	void __fastcall SetWriteMultiplier(int Value);
	void __fastcall SetWriteConstant(int Value);
	
public:
	__fastcall TTimeouts(TCustomComPort* ComPort);
	
__published:
	__property int ReadInterval = {read=FReadInterval, write=SetReadInterval, default=0};
	__property int ReadMultiplier = {read=FReadMultiplier, write=SetReadMultiplier, default=0};
	__property int ReadConstant = {read=FReadConstant, write=SetReadConstant, default=0};
	__property int WriteMultiplier = {read=FWriteMultiplier, write=SetWriteMultiplier, default=0};
	__property int WriteConstant = {read=FWriteConstant, write=SetWriteConstant, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTimeouts(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomComPort : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
	
private:
	typedef System::DynamicArray<System::Byte> _TCustomComPort__1;
	
	
private:
	bool FActive;
	NativeUInt FHandle;
	NativeUInt FReadEventHandle;
	NativeUInt FWriteEventHandle;
	_OVERLAPPED FEventOverlapped;
	_OVERLAPPED FReadOverlapped;
	_OVERLAPPED FWriteOverlapped;
	TComThread* FComThread;
	unsigned FEventMask;
	unsigned FRequestedReadCount;
	unsigned FRequestedWriteCount;
	void *FRequestedReadBuffer;
	void *FRequestedWriteBuffer;
	unsigned FOriginalPriorityClass;
	unsigned FThreadError;
	System::Classes::TThreadPriority FThreadPriority;
	int FCustomBaudRate;
	TBaudRate FBaudRate;
	_TCustomComPort__1 FBuffer;
	TBufferSizes* FBufferSizes;
	TCharacters* FCharacters;
	TDataBits FDataBits;
	System::UnicodeString FDeviceName;
	TFlowControl* FFlowControl;
	System::UnicodeString FLogFile;
	void *FNotificationHandle;
	TOptions FOptions;
	TParity FParity;
	TPriorityClass FPriorityClass;
	TStopBits FStopBits;
	bool FSynchronizeEvents;
	TTimeouts* FTimeouts;
	TOpenCloseEvent FAfterClose;
	TOpenCloseEvent FAfterOpen;
	TReadWriteEvent FAfterRead;
	TReadWriteEvent FAfterWrite;
	TOpenCloseEvent FBeforeClose;
	TOpenCloseEvent FBeforeOpen;
	TReadWriteEvent FBeforeRead;
	TReadWriteEvent FBeforeWrite;
	System::Classes::TNotifyEvent FOnBreak;
	System::Classes::TNotifyEvent FOnCTSChange;
	System::Classes::TNotifyEvent FOnDSRChange;
	TComErrorEvent FOnError;
	System::Classes::TNotifyEvent FOnEvent1;
	System::Classes::TNotifyEvent FOnEvent2;
	TLineErrorEvent FOnLineError;
	System::Classes::TNotifyEvent FOnPrinterError;
	System::Classes::TNotifyEvent FOnRing;
	System::Classes::TNotifyEvent FOnRLSDChange;
	System::Classes::TNotifyEvent FOnRx80PercFull;
	System::Classes::TNotifyEvent FOnRxChar;
	System::Classes::TNotifyEvent FOnRxFlag;
	System::Classes::TNotifyEvent FOnTxEmpty;
	TDeviceArrivalEvent FOnDeviceArrival;
	TDeviceRemovedEvent FOnDeviceRemoved;
	System::UnicodeString __fastcall GetAbout(void);
	bool __fastcall GetActive(void);
	int __fastcall GetCustomBaudRate(void);
	TLineErrors __fastcall GetLineErrors(unsigned Errors);
	TModemStatus __fastcall GetModemStatus(void);
	void __fastcall SetAbout(const System::UnicodeString Value);
	void __fastcall SetActive(const bool Value);
	void __fastcall SetBaudRate(TBaudRate Value);
	void __fastcall SetBufferSizes(TBufferSizes* Value);
	void __fastcall SetCharacters(TCharacters* Value);
	void __fastcall SetCustomBaudRate(int Value);
	void __fastcall SetDataBits(TDataBits Value);
	void __fastcall SetDeviceName(const System::UnicodeString Value);
	void __fastcall SetFlowControl(TFlowControl* Value);
	void __fastcall SetOptions(TOptions Value);
	void __fastcall SetParity(TParity Value);
	void __fastcall SetPriorityClass(TPriorityClass Value);
	void __fastcall SetStopBits(TStopBits Value);
	void __fastcall SetSynchronizeEvents(bool Value);
	void __fastcall SetThreadPriority(System::Classes::TThreadPriority Value);
	void __fastcall SetTimeouts(TTimeouts* Value);
	void __fastcall SetOnBreak(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnCTSChange(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnDSRChange(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnEvent1(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnEvent2(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnLineError(TLineErrorEvent Value);
	void __fastcall SetOnPrinterError(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnRing(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnRLSDChange(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnRx80PercFull(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnRxChar(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnRxFlag(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnTxEmpty(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnDeviceArrival(TDeviceArrivalEvent Value);
	void __fastcall SetOnDeviceRemoved(TDeviceRemovedEvent Value);
	void __fastcall CheckActive(void);
	void __fastcall CheckInactive(void);
	void __fastcall CopyDCB(_DCB &DCB, bool ToDCB);
	void __fastcall SetComDCB(void);
	void __fastcall SetComEventMask(void);
	void __fastcall SetComBufferSizes(void);
	void __fastcall SetComTimeouts(void);
	bool __fastcall StoreCustomBaudRate(void);
	System::UnicodeString __fastcall ShortDeviceName(void);
	bool __fastcall StartNotification(void);
	bool __fastcall StopNotification(void);
	bool __fastcall WindowHook(Winapi::Messages::TMessage &Message);
	
protected:
	virtual void __fastcall CreateHandle(void);
	void __fastcall FreeHandle(void);
	virtual void __fastcall Loaded(void);
	void __fastcall Check(bool Value);
	void __fastcall RaiseError(int ErrorCode, const System::UnicodeString ErrorMsg);
	void __fastcall RaiseThreadError(void);
	void __fastcall ThreadDoEvent(void);
	void __fastcall ThreadProc(TComThread* Thread);
	void __fastcall CreateThread(void);
	void __fastcall DestroyThread(void);
	bool __fastcall Log(void);
	void __fastcall WriteLog(const System::UnicodeString Message, void * Buf = (void *)(0x0), int Count = 0x0);
	__property System::UnicodeString About = {read=GetAbout, write=SetAbout, stored=false};
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__property TBaudRate BaudRate = {read=FBaudRate, write=SetBaudRate, default=0};
	__property TBufferSizes* BufferSizes = {read=FBufferSizes, write=SetBufferSizes};
	__property TCharacters* Characters = {read=FCharacters, write=SetCharacters};
	__property int CustomBaudRate = {read=GetCustomBaudRate, write=SetCustomBaudRate, stored=StoreCustomBaudRate, nodefault};
	__property TDataBits DataBits = {read=FDataBits, write=SetDataBits, default=0};
	__property System::UnicodeString DeviceName = {read=FDeviceName, write=SetDeviceName};
	__property TFlowControl* FlowControl = {read=FFlowControl, write=SetFlowControl};
	__property System::UnicodeString LogFile = {read=FLogFile, write=FLogFile};
	__property TModemStatus ModemStatus = {read=GetModemStatus, stored=false, nodefault};
	__property TOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property TParity Parity = {read=FParity, write=SetParity, default=0};
	__property TPriorityClass PriorityClass = {read=FPriorityClass, write=SetPriorityClass, default=0};
	__property TStopBits StopBits = {read=FStopBits, write=SetStopBits, default=0};
	__property bool SynchronizeEvents = {read=FSynchronizeEvents, write=SetSynchronizeEvents, default=1};
	__property System::Classes::TThreadPriority ThreadPriority = {read=FThreadPriority, write=SetThreadPriority, default=6};
	__property TTimeouts* Timeouts = {read=FTimeouts, write=SetTimeouts};
	__property TOpenCloseEvent AfterClose = {read=FAfterClose, write=FAfterClose};
	__property TOpenCloseEvent AfterOpen = {read=FAfterOpen, write=FAfterOpen};
	__property TReadWriteEvent AfterRead = {read=FAfterRead, write=FAfterRead};
	__property TReadWriteEvent AfterWrite = {read=FAfterWrite, write=FAfterWrite};
	__property TOpenCloseEvent BeforeClose = {read=FBeforeClose, write=FBeforeClose};
	__property TOpenCloseEvent BeforeOpen = {read=FBeforeOpen, write=FBeforeOpen};
	__property TReadWriteEvent BeforeRead = {read=FBeforeRead, write=FBeforeRead};
	__property TReadWriteEvent BeforeWrite = {read=FBeforeWrite, write=FBeforeWrite};
	__property System::Classes::TNotifyEvent OnBreak = {read=FOnBreak, write=SetOnBreak};
	__property System::Classes::TNotifyEvent OnCTSChange = {read=FOnCTSChange, write=SetOnCTSChange};
	__property System::Classes::TNotifyEvent OnDSRChange = {read=FOnDSRChange, write=SetOnDSRChange};
	__property TComErrorEvent OnError = {read=FOnError, write=FOnError};
	__property System::Classes::TNotifyEvent OnEvent1 = {read=FOnEvent1, write=SetOnEvent1};
	__property System::Classes::TNotifyEvent OnEvent2 = {read=FOnEvent2, write=SetOnEvent2};
	__property TLineErrorEvent OnLineError = {read=FOnLineError, write=SetOnLineError};
	__property System::Classes::TNotifyEvent OnPrinterError = {read=FOnPrinterError, write=SetOnPrinterError};
	__property System::Classes::TNotifyEvent OnRing = {read=FOnRing, write=SetOnRing};
	__property System::Classes::TNotifyEvent OnRLSDChange = {read=FOnRLSDChange, write=SetOnRLSDChange};
	__property System::Classes::TNotifyEvent OnRx80PercFull = {read=FOnRx80PercFull, write=SetOnRx80PercFull};
	__property System::Classes::TNotifyEvent OnRxChar = {read=FOnRxChar, write=SetOnRxChar};
	__property System::Classes::TNotifyEvent OnRxFlag = {read=FOnRxFlag, write=SetOnRxFlag};
	__property System::Classes::TNotifyEvent OnTxEmpty = {read=FOnTxEmpty, write=SetOnTxEmpty};
	__property TDeviceArrivalEvent OnDeviceArrival = {read=FOnDeviceArrival, write=SetOnDeviceArrival};
	__property TDeviceRemovedEvent OnDeviceRemoved = {read=FOnDeviceRemoved, write=SetOnDeviceRemoved};
	
public:
	__fastcall virtual TCustomComPort(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomComPort(void);
	void __fastcall Open(void);
	void __fastcall Close(void);
	void __fastcall Read(void * Buf, int Count, bool WaitForCompletion = false)/* overload */;
	void __fastcall Write(void * Buf, int Count, bool WaitForCompletion = false)/* overload */;
	System::UnicodeString __fastcall Read(System::Sysutils::TEncoding* Encoding)/* overload */;
	System::UnicodeString __fastcall ReadUtf8(void);
	System::DynamicArray<System::Byte> __fastcall ReadBytes(void);
	void __fastcall Write(const System::UnicodeString Value, System::Sysutils::TEncoding* Encoding)/* overload */;
	void __fastcall WriteUtf8(const System::UnicodeString Value);
	void __fastcall WriteBytes(const System::DynamicArray<System::Byte> Value);
	System::UnicodeString __fastcall ReadLine(System::Sysutils::TEncoding* Encoding)/* overload */;
	System::UnicodeString __fastcall ReadLineUtf8(void);
	System::DynamicArray<System::Byte> __fastcall ReadLineBytes(void);
	void __fastcall WriteLine(const System::UnicodeString Value, System::Sysutils::TEncoding* Encoding)/* overload */;
	void __fastcall WriteLineUtf8(const System::UnicodeString Value);
	void __fastcall WriteLineBytes(const System::DynamicArray<System::Byte> Value);
	System::UnicodeString __fastcall ReadUntil(System::Byte Terminator, System::Sysutils::TEncoding* Encoding)/* overload */;
	System::UnicodeString __fastcall ReadUntilUtf8(System::Byte Terminator);
	System::DynamicArray<System::Byte> __fastcall ReadUntilBytes(System::Byte Terminator);
	System::AnsiString __fastcall ReadAnsiUntil(char Terminator);
	System::AnsiString __fastcall ReadAnsiLine(void);
	void __fastcall WriteAnsiLine(const System::AnsiString Value);
	System::AnsiString __fastcall ReadAnsiString(void);
	void __fastcall WriteAnsiString(const System::AnsiString Value);
	char __fastcall ReadAnsiChar(void);
	void __fastcall WriteAnsiChar(char Value);
	System::UnicodeString __fastcall ReadUntil(System::WideChar Terminator)/* overload */;
	System::UnicodeString __fastcall ReadLine(void)/* overload */;
	void __fastcall WriteLine(const System::UnicodeString Value)/* overload */;
	System::UnicodeString __fastcall ReadString(void);
	void __fastcall WriteString(const System::UnicodeString Value);
	System::WideChar __fastcall ReadChar(void);
	void __fastcall WriteChar(System::WideChar Value);
	System::Byte __fastcall ReadByte(void);
	void __fastcall WriteByte(System::Byte Value);
	System::Word __fastcall ReadWord(void);
	void __fastcall WriteWord(System::Word Value);
	unsigned __fastcall ReadDWord(void);
	void __fastcall WriteDWord(unsigned Value);
	unsigned __fastcall InputCount(void);
	unsigned __fastcall OutputCount(void);
	bool __fastcall ReadPending(void);
	bool __fastcall WritePending(void);
	void __fastcall WaitForReadCompletion(void);
	void __fastcall WaitForWriteCompletion(void);
	void __fastcall PurgeInput(void);
	void __fastcall PurgeOutput(void);
	void __fastcall ClearInput(void);
	void __fastcall ClearOutput(void);
	void __fastcall AbortInput(void);
	void __fastcall AbortOutput(void);
	void __fastcall Flush(void);
	void __fastcall ClearLineErrors(void);
	void __fastcall CheckLineErrors(void);
	bool __fastcall ConfigDialog(void);
	bool __fastcall DefaultConfigDialog(void);
	void __fastcall EnumDevices(System::Classes::TStrings* List);
	void __fastcall EnumComDevices(System::Classes::TStrings* List);
	void __fastcall EnumComDevicesFromRegistry(System::Classes::TStrings* List);
	void __fastcall EnumComFriendlyNames(System::Classes::TStrings* List);
	void __fastcall ClearBreak(void);
	void __fastcall ClearDTR(void);
	void __fastcall ClearRTS(void);
	void __fastcall ResetDevice(void);
	void __fastcall SetBreak(void);
	void __fastcall SetDTR(void);
	void __fastcall SetRTS(void);
	void __fastcall SetXOn(void);
	void __fastcall SetXOff(void);
	void __fastcall TransmitChar(char Value);
	__property NativeUInt Handle = {read=FHandle, nodefault};
};


class PASCALIMPLEMENTATION TComPort : public TCustomComPort
{
	typedef TCustomComPort inherited;
	
__published:
	__property About = {default=0};
	__property Active;
	__property BaudRate = {default=0};
	__property BufferSizes;
	__property Characters;
	__property CustomBaudRate;
	__property DataBits = {default=0};
	__property DeviceName = {default=0};
	__property FlowControl;
	__property LogFile = {default=0};
	__property ModemStatus;
	__property Options;
	__property Parity = {default=0};
	__property PriorityClass = {default=0};
	__property StopBits = {default=0};
	__property SynchronizeEvents = {default=1};
	__property ThreadPriority = {default=6};
	__property Timeouts;
	__property AfterClose;
	__property AfterOpen;
	__property AfterRead;
	__property AfterWrite;
	__property BeforeClose;
	__property BeforeOpen;
	__property BeforeRead;
	__property BeforeWrite;
	__property OnBreak;
	__property OnCTSChange;
	__property OnDSRChange;
	__property OnError;
	__property OnEvent1;
	__property OnEvent2;
	__property OnLineError;
	__property OnPrinterError;
	__property OnRing;
	__property OnRLSDChange;
	__property OnRx80PercFull;
	__property OnRxChar;
	__property OnRxFlag;
	__property OnTxEmpty;
	__property OnDeviceArrival;
	__property OnDeviceRemoved;
public:
	/* TCustomComPort.Create */ inline __fastcall virtual TComPort(System::Classes::TComponent* AOwner) : TCustomComPort(AOwner) { }
	/* TCustomComPort.Destroy */ inline __fastcall virtual ~TComPort(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define SComOpen L"Can't perform this operation on an open port"
#define SComClosed L"Can't perform this operation on a closed port"
#define SComCantCreateThread L"Can't create thread"
#define SComTimeout L"Timeout"
}	/* namespace Comport */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_COMPORT)
using namespace Comport;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ComportHPP
