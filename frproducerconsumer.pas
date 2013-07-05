unit frproducerconsumer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,testproducerconsumer;

type

  { TFrameProducerConsumer }

  TFrameProducerConsumer = class(TFrame)
    ButtonRun: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
    procedure ButtonRunClick(Sender: TObject);
  private
    FTest : TProducerConsumer;
  public
  end;

implementation

{$R *.lfm}

{ TFrameProducerConsumer }

procedure TFrameProducerConsumer.ButtonRunClick(Sender: TObject);
begin
  FTest:=TProducerConsumer.Create
    (BoundedQueue,
     1024, // Queue length
     2,    // Sending threads
     2,    // Receiving threads
     1000, // Packets each sending thread
     0);   // Data each packet in bytes
end;

end.

