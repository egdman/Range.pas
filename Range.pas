{--------------------------------------------------------------------------------
  Purpose: Generic manipulations of enumerable objects.

  Copyright (c) Dmitry Egorov. https://github.com/egdman

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
--------------------------------------------------------------------------------}
unit Range;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  IRange<T> = interface(IInterface)
    {--------------------------------------------------------------------------------
      Descr: Returns the underlying TEnumerable implementation to be passed to
      functions that expect a TEnumerable.
    --------------------------------------------------------------------------------}
    function Items(): TEnumerable<T>;

    {--------------------------------------------------------------------------------
      Descr: The special method that makes IRange iterable in loops.
    --------------------------------------------------------------------------------}
    function GetEnumerator(): TEnumerator<T>;
  end;

  IStream<T> = interface(IRange<T>)
    {--------------------------------------------------------------------------------
      Descr: Gets the next item from the stream if it exists, otherwise raises
      EStreamEnded.
    --------------------------------------------------------------------------------}
    function Next(): T;

    {--------------------------------------------------------------------------------
      Descr: If the next item in the stream exists, sets AItem and returns true,
      otherwise returns false.
    --------------------------------------------------------------------------------}
    function TryNext(var AItem: T): Boolean;

    {--------------------------------------------------------------------------------
      Descr: Gets the next item from the stream if it exists, otherwise returns
      the given default value.
    --------------------------------------------------------------------------------}
    function NextOr(const ADefault: T): T;
  end;

  EStreamEnded = class(Exception);

  TRange<T> = record
  {--------------------------------------------------------------------------------
    Descr: A variant type that can hold a reference to an IRange or a reference
    to a TEnumerable.
  --------------------------------------------------------------------------------}
  private
    FLifetime: IRange<T>;
    FItems: TEnumerable<T>;
  public
    class operator Implicit(const AValue: IRange<T>): TRange<T>;
    class operator Implicit(const AValue: TEnumerable<T>): TRange<T>;

    {--------------------------------------------------------------------------------
      Descr: The special method that makes TRange iterable in loops.
    --------------------------------------------------------------------------------}
    function GetEnumerator(): TEnumerator<T>;

    property Lifetime: IRange<T> read FLifetime;
    property Items: TEnumerable<T> read FItems;
  end;


  TSymbols = class abstract
    type TEmpty = (_);
  end;


  TRange = class abstract
  public
    type
      TItemTransformation<T_src, T_dest> = reference to function(const ASrc: T_src): T_dest;

      TItemPredicate<T> = reference to function(const ASrc: T): Boolean;

      TZipper<T_left, T_right, T_out> = reference to function(
        const ALeft: T_left; const ARight: T_right): T_out;

      TTerminatorCallback<T> = reference to procedure(const AItem: T);

    {--------------------------------------------------------------------------------
      Descr: Returns an empty stream.
    --------------------------------------------------------------------------------}
    class function Empty<T>(): IStream<T>;

    {--------------------------------------------------------------------------------
      Descr: Returns numbers AStart, AStart+1, AStart+2, ... etc.
    --------------------------------------------------------------------------------}
    class function Iota(const AStart: NativeUInt = 0): IStream<NativeUInt>;

    {--------------------------------------------------------------------------------
      Descr: Returns at most ACount first items of AInput.
    --------------------------------------------------------------------------------}
    class function Head<T>(const AInput: TRange<T>; const ACount: NativeUInt): IStream<T>;

    {--------------------------------------------------------------------------------
      Descr: Applies the given function ATransformation to the input elements and
      returns a stream containing the return values.
    --------------------------------------------------------------------------------}
    class function Map<T_src, T_dest>(
      const AInput: TRange<T_src>;
      const ATransformation: TItemTransformation<T_src, T_dest>): IStream<T_dest>; overload;
    class function Map<T_src, T_dest>(
      const AInput: TArray<T_src>;
      const ATransformation: TItemTransformation<T_src, T_dest>): IStream<T_dest>; overload;

    {--------------------------------------------------------------------------------
      Descr: Calls APredicate for each input item and returns only those that
      evaluated to True.
    --------------------------------------------------------------------------------}
    class function Filter<T>(
      const AInput: TRange<T>;
      const APredicate: TItemPredicate<T>): IStream<T>; overload;
    class function Filter<T>(
      const AInput: TArray<T>;
      const APredicate: TItemPredicate<T>): IStream<T>; overload;

    {--------------------------------------------------------------------------------
      Descr: Similar to Filter, except it stops at the first input item that
      evaluates to False.

      The first item that evaluates to False, called the "terminator", will be
      consumed from the input range. If AIncludeTerminator is True, that item will
      be included at the end of the output stream, otherwise it will be lost.

      Alternatively, you can provide ACallback, which will be called with the
      terminator as the only argument. In this case it will not be included in the
      output stream.
    --------------------------------------------------------------------------------}
    class function TakeWhile<T>(
      const AInput: TRange<T>;
      const APredicate: TItemPredicate<T>;
      const AIncludeTerminator: Boolean = False): IStream<T>; overload;
    class function TakeWhile<T>(
      const AInput: TArray<T>;
      const APredicate: TItemPredicate<T>;
      const AIncludeTerminator: Boolean = False): IStream<T>; overload;
    class function TakeWhile<T>(
      const AInput: TRange<T>;
      const APredicate: TItemPredicate<T>;
      const ACallback: TTerminatorCallback<T>): IStream<T>; overload;
    class function TakeWhile<T>(
      const AInput: TArray<T>;
      const APredicate: TItemPredicate<T>;
      const ACallback: TTerminatorCallback<T>): IStream<T>; overload;

    {--------------------------------------------------------------------------------
      Descr: Iterates over both given ranges in parallel and calls AZipper for the
      item pairs to construct the output values.

      The iteration ends when the shorter of the two ranges ends. If ARight is
      shorter, the first unpaired left item will be lost. To retain the first
      unpaired left item, please provide ACallback procedure, which will be called
      with that item as the only argument.
    --------------------------------------------------------------------------------}
    class function Zip<T_left, T_right, T_out>(
      const ALeft: TRange<T_left>;
      const ARight: TRange<T_right>;
      const AZipper: TZipper<T_left, T_right, T_out>): IStream<T_out>; overload;
    class function Zip<T_left, T_right, T_out>(
      const ALeft: TRange<T_left>;
      const ARight: TRange<T_right>;
      const AZipper: TZipper<T_left, T_right, T_out>;
      const ACallback: TTerminatorCallback<T_left>): IStream<T_out>; overload;

    {--------------------------------------------------------------------------------
      Descr: Chains multiple ranges into a single range.
    --------------------------------------------------------------------------------}
    class function Chain<T>(const ARanges: TRange<TRange<T>>): IStream<T>; overload;
    class function Chain<T>(const ARanges: TArray<TRange<T>>): IStream<T>; overload;

    {--------------------------------------------------------------------------------
      Descr: Wraps a TEnumerable into an IRange/IStream object. The returned object
      does not manage the lifetime of the TEnumerable.
    --------------------------------------------------------------------------------}
    class function AsRange<T>(const ARange: TEnumerable<T>): IRange<T>; overload;
    class function AsStream<T>(const ARange: TEnumerable<T>): IStream<T>; overload;

    {--------------------------------------------------------------------------------
      Descr: Wraps an array into an IRange/IStream object.
    --------------------------------------------------------------------------------}
    class function AsRange<T>(const AArray: TArray<T>): IRange<T>; overload;
    class function AsStream<T>(const AArray: TArray<T>): IStream<T>; overload;

    const _EMPTY = TSymbols.TEmpty._;
  end;

  TReferenceCounter = class;

  TAutomaticRange<T> = class abstract(TEnumerable<T>, IRange<T>)
  {--------------------------------------------------------------------------------
    Descr: A reference-counted enumerable range.
  --------------------------------------------------------------------------------}
  private
    FRefCounter: TReferenceCounter;
  protected
    function QueryInterface(const AInterfaceId: TGUID; out AObject): HResult; stdcall;
    function _AddRef(): Integer; stdcall;
    function _Release(): Integer; stdcall;
    function Items(): TEnumerable<T>;
  public
    class function NewInstance(): TObject; override;
  end;


  TReferenceCounter = class(TInterfacedObject, IInterface)
  public
    function QueryInterface(const AInterfaceId: TGUID; out AObject): HResult; stdcall;
    function _AddRef(): Integer; stdcall;
    function _Release(): Integer; stdcall;
  end;


  TAutomaticRangeWithCleanup<T> = class abstract(TAutomaticRange<T>)
  protected
    type TCleanupProcedure = reference to procedure();
  protected
    FCleanup: TCleanupProcedure;
  public
    destructor Destroy(); override;
  end;


  TStatelessRange<T> = class(TAutomaticRangeWithCleanup<T>)
  private
    type
      TFactory = reference to function(): TEnumerator<T>;
      TCleanupProcedure = TAutomaticRangeWithCleanup<T>.TCleanupProcedure;
  protected
    FEnumeratorFactory: TFactory;

    function DoGetEnumerator(): TEnumerator<T>; override;
  public
    constructor Create(const _: TSymbols.TEmpty); overload; // constructs an empty range
    constructor Create(const AFactory: TFactory); overload;
    function WithCleanup(const ACleanupProcedure: TCleanupProcedure): TStatelessRange<T>;
  end;


  TStatefulRange<T> = class(TAutomaticRangeWithCleanup<T>, IStream<T>)
  private
    type
      TCleanupProcedure = TAutomaticRangeWithCleanup<T>.TCleanupProcedure;
  private
    FSource: IInterface;

  protected
    FEnumerator: TEnumerator<T>;

    function DoGetEnumerator(): TEnumerator<T>; override;
  public
    constructor Create(const _: TSymbols.TEmpty); overload; // constructs an empty range
    constructor Create(
      const ASource: IInterface;
      AStateEnumerator: TEnumerator<T>); overload;
    destructor Destroy(); override;
    function WithCleanup(const ACleanupProcedure: TCleanupProcedure): TStatefulRange<T>;

    function Next(): T;
    function TryNext(var AItem: T): Boolean;
    function NextOr(const ADefault: T): T;
  end;


  TGenerator = class abstract
    type TResumeState = (
      rsInit,
      rsFinish,
      rsContinue,
      rsContinue1,
      rsContinue2,
      rsContinue3,
      rsContinue4,
      rsContinue5,
      rsContinue6,
      rsContinue7,
      rsContinue8,
      rsContinue9);
  end;


  TEnumeratorBase<T> = class abstract(TEnumerator<T>)
  protected
    FCurrent: T;

    function DoGetCurrent(): T; override;
  public
    property Current: T read FCurrent;
  end;


  TGenerator<T> = class abstract(TEnumeratorBase<T>)
  public
    type TResumeState = TGenerator.TResumeState;
  private
    FState: TResumeState;

  protected
    function Yield(const AValue: T; const AState: TResumeState): Boolean; // always returns True
    function YieldLast(const AValue: T): Boolean; // always returns True
    function SignalStop(): Boolean; // always returns False
  public
    constructor Create(const _: TSymbols.TEmpty); overload;
    constructor Create(); overload;

    property State: TResumeState read FState;
  end;


  TFunctionEnumerator<T> = class abstract(TEnumeratorBase<T>)
  private
    type
      TEnumeratorFunc = reference to function(var ACurrent: T): Boolean;
  private
    FStateFunc: TEnumeratorFunc;

  protected
    function DoMoveNext(): Boolean; override;
    function StateFunc(var ACurrent: T): Boolean; virtual; abstract;
    class function TerminalState(var ACurrent: T): Boolean;

    function SignalLast(): Boolean; // always returns True
    function SignalStop(): Boolean; // always returns False
  public
    constructor Create(const _: TSymbols.TEmpty); overload;
    constructor Create(); overload;
  end;


  TEmptyEnumerator<T> = class(TFunctionEnumerator<T>)
  protected
    function StateFunc(var ACurrent: T): Boolean; override;
  end;


  TCountingEnumerator = class(TFunctionEnumerator<NativeUInt>)
  protected
    function StateFunc(var ACurrent: NativeUInt): Boolean; override;
    function DoGetCurrent(): NativeUInt; override;
  public
    constructor Create(const AStart: NativeUInt);
  end;


  TEnumeratorDelegate<T> = class(TEnumerator<T>)
  private
    FOriginal: TEnumerator<T>;

  protected
    function DoGetCurrent(): T; override;
    function DoMoveNext(): Boolean; override;
  public
    constructor Create(const AOriginal: TEnumerator<T>);
    property Current: T read DoGetCurrent;
  end;


  TMappingEnumerator<T_src, T_dest> = class(TFunctionEnumerator<T_dest>)
  private
    FEnumerator: TEnumerator<T_src>;
    FTransformation: TRange.TItemTransformation<T_src, T_dest>;

  protected
    function StateFunc(var ACurrent: T_dest): Boolean; override;
  public
    constructor Create(
      const AInput: TEnumerator<T_src>;
      const ATransformation: TRange.TItemTransformation<T_src, T_dest>); overload;
    constructor Create(
      const AInput: TArray<T_src>;
      const ATransformation: TRange.TItemTransformation<T_src, T_dest>); overload;

    destructor Destroy(); override;
  end;


  TFilteringEnumerator<T> = class(TFunctionEnumerator<T>)
  private
    FEnumerator: TEnumerator<T>;
    FPredicate: TRange.TItemPredicate<T>;
  protected
    function StateFunc(var ACurrent: T): Boolean; override;
  public
    constructor Create(
      const AInput: TEnumerator<T>;
      const APredicate: TRange.TItemPredicate<T>); overload;
    constructor Create(
      const AInput: TArray<T>;
      const APredicate: TRange.TItemPredicate<T>); overload;

    destructor Destroy(); override;
  end;


  TPredicateEnumerator<T> = class(TFilteringEnumerator<T>)
  private
    FCallback: TRange.TTerminatorCallback<T>;
    FIncludeTerminator: Boolean;
  protected
    function StateFunc(var ACurrent: T): Boolean; override;
  public
    function WithTerminatorCallback(const ACallback: TRange.TTerminatorCallback<T>): TPredicateEnumerator<T>;
    function IncludeTerminator(const AValue: Boolean): TPredicateEnumerator<T>;
  end;


  TZippingEnumerator<T_left, T_right, T_out> = class(TFunctionEnumerator<T_out>)
  private
    FInputLeft: IRange<T_left>;
    FInputRight: IRange<T_right>;
    FEnumeratorLeft: TEnumerator<T_left>;
    FEnumeratorRight: TEnumerator<T_right>;
    FZipper: TRange.TZipper<T_left, T_right, T_out>;
    FCallback: TRange.TTerminatorCallback<T_left>;

  protected
    function StateFunc(var ACurrent: T_out): Boolean; override;
  public
    constructor Create(
      const ALeft: TRange<T_left>;
      const ARight: TRange<T_right>;
      const AZipper: TRange.TZipper<T_left, T_right, T_out>);

    destructor Destroy(); override;
    function WithTerminatorCallback(const ACallback: TRange.TTerminatorCallback<T_left>): TZippingEnumerator<T_left, T_right, T_out>;
  end;


  TChainEnumerator<T> = class(TFunctionEnumerator<T>)
  private
    FEnumerator: TEnumerator<T>;
    FArgEnumerator: TEnumerator<TRange<T>>;

  protected
    function StateFunc(var ACurrent: T): Boolean; override;
  public
    constructor Create(const ARanges: TEnumerator<TRange<T>>); overload;
    constructor Create(const ARanges: TArray<TRange<T>>); overload;
    destructor Destroy(); override;
  end;


  TArrayEnumerator<T> = class(TEnumeratorBase<T>)
  private
    FArray: TArray<T>;
    FIndex: NativeUInt;

  protected
    function DoMoveNext(): Boolean; override;
  public
    constructor Create(const AArray: TArray<T>);
  end;


function AdvancePost(var AValue: NativeUInt): NativeUInt;

implementation


function AdvancePost(var AValue: NativeUInt): NativeUInt;
begin
  Result := AValue;
  Inc(AValue);
end;

{ TEnumeratorBase }

function TEnumeratorBase<T>.DoGetCurrent(): T;
begin
  Result := FCurrent;
end;

{ TGenerator }

constructor TGenerator<T>.Create();
begin
  inherited;

  FState := rsInit;
end;

constructor TGenerator<T>.Create(const _: TSymbols.TEmpty);
begin
  inherited Create();

  FState := rsFinish;
end;

function TGenerator<T>.Yield(const AValue: T; const AState: TResumeState): Boolean;
begin
  FCurrent := AValue;
  FState := AState;
  Result := True;
end;

function TGenerator<T>.YieldLast(const AValue: T): Boolean;
begin
  FCurrent := AValue;
  FState := rsFinish;
  Result := True;
end;

function TGenerator<T>.SignalStop(): Boolean;
begin
  FState := rsFinish;
  Result := False;
end;

{ TFunctionEnumerator }

constructor TFunctionEnumerator<T>.Create();
begin
  inherited Create();

  FStateFunc := StateFunc;
end;

constructor TFunctionEnumerator<T>.Create(const _: TSymbols.TEmpty);
begin
  inherited Create();

  FStateFunc := TerminalState;
end;

function TFunctionEnumerator<T>.SignalLast(): Boolean;
begin
  FStateFunc := TerminalState;
  Result := True;
end;

function TFunctionEnumerator<T>.SignalStop(): Boolean;
begin
  FStateFunc := TerminalState;
  Result := False;
end;

function TFunctionEnumerator<T>.DoMoveNext(): Boolean;
begin
  Result := FStateFunc(FCurrent);
end;

class function TFunctionEnumerator<T>.TerminalState(var ACurrent: T): Boolean;
begin
  Result := False;
end;

{ TEmptyEnumerator }

function TEmptyEnumerator<T>.StateFunc(var ACurrent: T): Boolean;
begin
  Result := SignalStop();
end;

{ TEnumeratorDelegate }

constructor TEnumeratorDelegate<T>.Create(const AOriginal: TEnumerator<T>);
begin
  inherited Create();

  FOriginal := AOriginal;
end;

function TEnumeratorDelegate<T>.DoMoveNext(): Boolean;
begin
  Result := FOriginal.MoveNext();
end;

function TEnumeratorDelegate<T>.DoGetCurrent(): T;
begin
  Result := FOriginal.Current;
end;

{ TCountingEnumerator }

constructor TCountingEnumerator.Create(const AStart: NativeUInt);
begin
  inherited Create();

  FCurrent := AStart;
end;

function TCountingEnumerator.StateFunc(var ACurrent: NativeUInt): Boolean;
begin
  ACurrent := Current + 1;
  Result := True;
end;

function TCountingEnumerator.DoGetCurrent(): NativeUInt;
begin
  Result := Current - 1;
end;

{ TMappingEnumerator }

constructor TMappingEnumerator<T_src, T_dest>.Create(
  const AInput: TEnumerator<T_src>;
  const ATransformation: TRange.TItemTransformation<T_src, T_dest>);
begin
  inherited Create();

  FEnumerator := AInput;
  FTransformation := ATransformation;
end;

constructor TMappingEnumerator<T_src, T_dest>.Create(
  const AInput: TArray<T_src>;
  const ATransformation: TRange.TItemTransformation<T_src, T_dest>);
begin
  inherited Create();

  FEnumerator := TArrayEnumerator<T_src>.Create(AInput);
  FTransformation := ATransformation;
end;

destructor TMappingEnumerator<T_src, T_dest>.Destroy();
begin
  FreeAndNil(FEnumerator);

  inherited;
end;

function TMappingEnumerator<T_src, T_dest>.StateFunc(var ACurrent: T_dest): Boolean;
begin
  if not FEnumerator.MoveNext() then
    EXIT(SignalStop());

  ACurrent := FTransformation(FEnumerator.Current);
  EXIT(True);
end;

{ TFilteringEnumerator }

constructor TFilteringEnumerator<T>.Create(
  const AInput: TEnumerator<T>;
  const APredicate: TRange.TItemPredicate<T>);
begin
  inherited Create();

  FEnumerator := AInput;
  FPredicate := APredicate;
end;

constructor TFilteringEnumerator<T>.Create(
  const AInput: TArray<T>;
  const APredicate: TRange.TItemPredicate<T>);
begin
  inherited Create();

  FEnumerator := TArrayEnumerator<T>.Create(AInput);
  FPredicate := APredicate;
end;

function TFilteringEnumerator<T>.StateFunc(var ACurrent: T): Boolean;
begin
  while FEnumerator.MoveNext() do
  begin
    if not FPredicate(FEnumerator.Current) then
      CONTINUE;

    ACurrent := FEnumerator.Current;
    EXIT(True);
  end;

  EXIT(SignalStop());
end;

destructor TFilteringEnumerator<T>.Destroy();
begin
  FreeAndNil(FEnumerator);

  inherited;
end;

{ TPredicateEnumerator }

function TPredicateEnumerator<T>.StateFunc(var ACurrent: T): Boolean;
begin
  if not FEnumerator.MoveNext() then
    EXIT(SignalStop());

  ACurrent := FEnumerator.Current;
  if FPredicate(FEnumerator.Current) then
    EXIT(True)

  else if FIncludeTerminator then
    EXIT(SignalLast())

  else if Assigned(FCallback) then
    FCallback(FEnumerator.Current);

  EXIT(SignalStop());
end;

function TPredicateEnumerator<T>.WithTerminatorCallback(const ACallback: TRange.TTerminatorCallback<T>): TPredicateEnumerator<T>;
begin
  FCallback := ACallback;
  Result := Self;
end;

function TPredicateEnumerator<T>.IncludeTerminator(const AValue: Boolean): TPredicateEnumerator<T>;
begin
  FIncludeTerminator := AValue;
  Result := Self;
end;

{ TZippingEnumerator }

constructor TZippingEnumerator<T_left, T_right, T_out>.Create(
  const ALeft: TRange<T_left>;
  const ARight: TRange<T_right>;
  const AZipper: TRange.TZipper<T_left, T_right, T_out>);
begin
  inherited Create();

  FInputLeft := ALeft.Lifetime;
  FInputRight := ARight.Lifetime;
  FEnumeratorLeft := ALeft.GetEnumerator();
  FEnumeratorRight := ARight.GetEnumerator();
  FZipper := AZipper;
end;

destructor TZippingEnumerator<T_left, T_right, T_out>.Destroy();
begin
  FreeAndNil(FEnumeratorRight);
  FreeAndNil(FEnumeratorLeft);

  inherited;
end;

function TZippingEnumerator<T_left, T_right, T_out>.StateFunc(var ACurrent: T_out): Boolean;
begin
  if FEnumeratorLeft.MoveNext() then
  begin
    if FEnumeratorRight.MoveNext() then
    begin
      ACurrent := FZipper(FEnumeratorLeft.Current, FEnumeratorRight.Current);
      EXIT(True);
    end;

    if Assigned(FCallback) then
      FCallback(FEnumeratorLeft.Current);
  end;
  EXIT(SignalStop());
end;

function TZippingEnumerator<T_left, T_right, T_out>.WithTerminatorCallback(
  const ACallback: TRange.TTerminatorCallback<T_left>): TZippingEnumerator<T_left, T_right, T_out>;
begin
  FCallback := ACallback;
  Result := Self;
end;

{ TArrayEnumerator }

constructor TArrayEnumerator<T>.Create(const AArray: TArray<T>);
begin
  inherited Create();

  FArray := AArray;
  FIndex := Low(AArray);
end;

function TArrayEnumerator<T>.DoMoveNext(): Boolean;
begin
  Result := FIndex <= High(FArray);
  if not Result then
    EXIT(False);

  FCurrent := FArray[AdvancePost(FIndex)];
end;

{ TChainEnumerator }

constructor TChainEnumerator<T>.Create(const ARanges: TEnumerator<TRange<T>>);
begin
  inherited Create();

  FArgEnumerator := ARanges;
  if FArgEnumerator.MoveNext() then
    FEnumerator := FArgEnumerator.Current.GetEnumerator()
  else
    SignalStop();
end;

constructor TChainEnumerator<T>.Create(const ARanges: TArray<TRange<T>>);
begin
  inherited Create();

  FArgEnumerator := TArrayEnumerator<TRange<T>>.Create(ARanges);
  if FArgEnumerator.MoveNext() then
    FEnumerator := FArgEnumerator.Current.GetEnumerator()
  else
    SignalStop();
end;

destructor TChainEnumerator<T>.Destroy();
begin
  FreeAndNil(FEnumerator);
  FreeAndNil(FArgEnumerator);

  inherited;
end;

function TChainEnumerator<T>.StateFunc(var ACurrent: T): Boolean;
begin
  while True do
  begin
    if FEnumerator.MoveNext() then
    begin
      ACurrent := FEnumerator.Current;
      EXIT(True);
    end
    else if FArgEnumerator.MoveNext() then
    begin
      FreeAndNil(FEnumerator);
      FEnumerator := FArgEnumerator.Current.GetEnumerator();
    end
    else
      EXIT(SignalStop());
  end;
end;

{ TReferenceCounter }

function TReferenceCounter.QueryInterface(const AInterfaceId: TGUID; out AObject): HResult;
begin
  Result := inherited;
end;

function TReferenceCounter._AddRef(): Integer;
begin
  Result := inherited;
end;

function TReferenceCounter._Release(): Integer;
begin
  Result := inherited;
end;

{ TAutomaticRange }

class function TAutomaticRange<T>.NewInstance(): TObject;
begin
  Result := inherited NewInstance();
  TAutomaticRange<T>(Result).FRefCounter := TReferenceCounter.Create();
end;

function TAutomaticRange<T>.QueryInterface(const AInterfaceId: TGUID; out AObject): HResult;
begin
  Result := FRefCounter.QueryInterface(AInterfaceId, AObject);
end;

function TAutomaticRange<T>._AddRef(): Integer;
begin
  Result := FRefCounter._AddRef();
end;

function TAutomaticRange<T>._Release(): Integer;
begin
  Result := FRefCounter._Release();

  if Result = 0 then
  begin
    FRefCounter := nil; // This object should already be destroyed
    Destroy();
  end;
end;

function TAutomaticRange<T>.Items(): TEnumerable<T>;
begin
  Result := Self;
end;

{ TAutomaticRangeWithCleanup }

destructor TAutomaticRangeWithCleanup<T>.Destroy();
begin
  if Assigned(FCleanup) then
    FCleanup();

  inherited;
end;

{ TStatelessRange }

constructor TStatelessRange<T>.Create(const _: TSymbols.TEmpty);
begin
  inherited Create();

  FEnumeratorFactory := function(): TEnumerator<T>
    begin
      Result := TEmptyEnumerator<T>.Create();
    end;
end;

constructor TStatelessRange<T>.Create(const AFactory: TFactory);
begin
  inherited Create();

  FEnumeratorFactory := AFactory;
end;

function TStatelessRange<T>.DoGetEnumerator(): TEnumerator<T>;
begin
  Result := FEnumeratorFactory();
end;

function TStatelessRange<T>.WithCleanup(const ACleanupProcedure: TCleanupProcedure): TStatelessRange<T>;
begin
  Result := Self;
  if Assigned(FCleanup) then
    FCleanup();
  FCleanup := ACleanupProcedure;
end;

{ TStatefulRange }

constructor TStatefulRange<T>.Create(const _: TSymbols.TEmpty);
begin
  inherited Create();

  FEnumerator := TEmptyEnumerator<T>.Create();
end;

constructor TStatefulRange<T>.Create(
  const ASource: IInterface;
  AStateEnumerator: TEnumerator<T>);
begin
  inherited Create();

  FSource := ASource;
  FEnumerator := AStateEnumerator;
end;

destructor TStatefulRange<T>.Destroy();
begin
  FreeAndNil(FEnumerator);

  inherited;
end;

function TStatefulRange<T>.WithCleanup(const ACleanupProcedure: TCleanupProcedure): TStatefulRange<T>;
begin
  Result := Self;
  if Assigned(FCleanup) then
    FCleanup();
  FCleanup := ACleanupProcedure;
end;

function TStatefulRange<T>.DoGetEnumerator(): TEnumerator<T>;
begin
  Result := TEnumeratorDelegate<T>.Create(FEnumerator);
end;

function TStatefulRange<T>.Next(): T;
begin
  if not FEnumerator.MoveNext() then
    raise EStreamEnded.Create('stream ended');

  Result := FEnumerator.Current;
end;

function TStatefulRange<T>.TryNext(var AItem: T): Boolean;
begin
  Result := FEnumerator.MoveNext();
  if Result then
    AItem := FEnumerator.Current;
end;

function TStatefulRange<T>.NextOr(const ADefault: T): T;
begin
  if FEnumerator.MoveNext() then
    Result := FEnumerator.Current
  else
    Result := ADefault;
end;

{ TRange<T> }

class operator TRange<T>.Implicit(const AValue: IRange<T>): TRange<T>;
begin
  Result.FLifetime := AValue;
  Result.FItems := AValue.Items();
end;

class operator TRange<T>.Implicit(const AValue: TEnumerable<T>): TRange<T>;
begin
  Result.FLifetime := nil;
  Result.FItems := AValue;
end;

function TRange<T>.GetEnumerator(): TEnumerator<T>;
begin
  Result := FItems.GetEnumerator();
end;

{ TRange }

class function TRange.Empty<T>(): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(_EMPTY);
end;

class function TRange.Iota(const AStart: NativeUInt): IStream<NativeUInt>;
begin
  Result := TStatefulRange<NativeUInt>.Create(nil, TCountingEnumerator.Create(AStart));
end;

class function TRange.Head<T>(const AInput: TRange<T>; const ACount: NativeUInt): IStream<T>;
const
  INCL_TERMINATOR = True;
var
  LCount: NativeUInt;
begin
  if ACount < 1 then
    EXIT(Empty<T>());

  LCount := 1;
  Result := TakeWhile<T>(AInput, function(const AItem: T): Boolean
    begin
      Result := AdvancePost(LCount) < ACount;
    end, INCL_TERMINATOR);
end;

class function TRange.Map<T_src, T_dest>(
  const AInput: TRange<T_src>;
  const ATransformation: TItemTransformation<T_src, T_dest>): IStream<T_dest>;
begin
  Result := TStatefulRange<T_dest>.Create(AInput.Lifetime,
    TMappingEnumerator<T_src, T_dest>.Create(AInput.GetEnumerator(), ATransformation));
end;

class function TRange.Map<T_src, T_dest>(
  const AInput: TArray<T_src>;
  const ATransformation: TItemTransformation<T_src, T_dest>): IStream<T_dest>;
begin
  Result := TStatefulRange<T_dest>.Create(nil,
    TMappingEnumerator<T_src, T_dest>.Create(AInput, ATransformation));
end;

class function TRange.Filter<T>(
  const AInput: TRange<T>;
  const APredicate: TItemPredicate<T>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(AInput.Lifetime,
    TFilteringEnumerator<T>.Create(AInput.GetEnumerator(), APredicate))
end;

class function TRange.Filter<T>(
  const AInput: TArray<T>;
  const APredicate: TItemPredicate<T>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(nil,
    TFilteringEnumerator<T>.Create(AInput, APredicate));
end;

class function TRange.TakeWhile<T>(
  const AInput: TRange<T>;
  const APredicate: TItemPredicate<T>;
  const AIncludeTerminator: Boolean): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(AInput.Lifetime,
    TPredicateEnumerator<T>.Create(AInput.GetEnumerator(), APredicate)
    .IncludeTerminator(AIncludeTerminator));
end;

class function TRange.TakeWhile<T>(
  const AInput: TArray<T>;
  const APredicate: TItemPredicate<T>;
  const AIncludeTerminator: Boolean): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(nil,
    TPredicateEnumerator<T>.Create(AInput, APredicate).IncludeTerminator(AIncludeTerminator));
end;

class function TRange.TakeWhile<T>(
  const AInput: TRange<T>;
  const APredicate: TItemPredicate<T>;
  const ACallback: TTerminatorCallback<T>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(AInput.Lifetime,
    TPredicateEnumerator<T>.Create(AInput.GetEnumerator(), APredicate)
    .WithTerminatorCallback(ACallback));
end;

class function TRange.TakeWhile<T>(
  const AInput: TArray<T>;
  const APredicate: TItemPredicate<T>;
  const ACallback: TTerminatorCallback<T>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(nil,
    TPredicateEnumerator<T>.Create(AInput, APredicate).WithTerminatorCallback(ACallback));
end;

class function TRange.Zip<T_left, T_right, T_out>(
  const ALeft: TRange<T_left>;
  const ARight: TRange<T_right>;
  const AZipper: TZipper<T_left, T_right, T_out>): IStream<T_out>;
begin
  Result := TStatefulRange<T_out>.Create(nil,
    TZippingEnumerator<T_left, T_right, T_out>.Create(ALeft, ARight, AZipper));
end;

class function TRange.Zip<T_left, T_right, T_out>(
  const ALeft: TRange<T_left>;
  const ARight: TRange<T_right>;
  const AZipper: TZipper<T_left, T_right, T_out>;
  const ACallback: TTerminatorCallback<T_left>): IStream<T_out>;
begin
  Result := TStatefulRange<T_out>.Create(nil,
    TZippingEnumerator<T_left, T_right, T_out>.Create(ALeft, ARight, AZipper)
    .WithTerminatorCallback(ACallback));
end;

class function TRange.Chain<T>(const ARanges: TRange<TRange<T>>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(ARanges.Lifetime,
    TChainEnumerator<T>.Create(ARanges.GetEnumerator()));
end;

class function TRange.Chain<T>(const ARanges: TArray<TRange<T>>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(nil, TChainEnumerator<T>.Create(ARanges));
end;

class function TRange.AsRange<T>(const ARange: TEnumerable<T>): IRange<T>;
begin
  Result := TStatelessRange<T>.Create(function(): TEnumerator<T>
    begin
      Result := ARange.GetEnumerator();
    end);
end;

class function TRange.AsStream<T>(const ARange: TEnumerable<T>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(nil, ARange.GetEnumerator());
end;

class function TRange.AsRange<T>(const AArray: TArray<T>): IRange<T>;
begin
  Result := TStatelessRange<T>.Create(function(): TEnumerator<T>
    begin
      Result := TArrayEnumerator<T>.Create(AArray);
    end);
end;

class function TRange.AsStream<T>(const AArray: TArray<T>): IStream<T>;
begin
  Result := TStatefulRange<T>.Create(nil, TArrayEnumerator<T>.Create(AArray));
end;

end.
