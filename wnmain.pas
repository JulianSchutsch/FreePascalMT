unit wnmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, uconcurrency,frproducerconsumer;

type

  { TWMain }

  TWMain = class(TForm)
    FrameProducerConsumer1: TFrameProducerConsumer;
    PageControl1: TPageControl;
    ProducerConsumerPage: TTabSheet;
  private
  public
  end;

var
  WMain: TWMain;

implementation

{$R *.lfm}

{ TWMain }

end.

