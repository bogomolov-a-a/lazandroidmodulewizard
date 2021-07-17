unit registerPackageUnit;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
   {$ifdef unix}BaseUnix,{$endif}
  android.wizard.project.descriptor.main,
  android.wizard.project.descriptor.gui;

procedure Register;
begin
  // TAndroidWidgetMediatorFactory.RegisterAndroidWidgetMediator();
  TAndroidProjectDescriptorFactory.RegisterAndroidProjectDescriptor();
  TGUIAndroidModuleFactory.RegisterGUIAndroidModuleResources();
end;

end.
