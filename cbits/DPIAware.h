#ifndef DPIAWARE_H
#define DPIAWARE_H

#include <windows.h>

void InitialiseDPIAwareFunctions(void);

HICON GetHighDPIIcon(SHSTOCKICONID siid);

double GetScaleFactorForHWND(HWND hwnd);

int ScaleValue(double scaleFactor, int v);

#endif
