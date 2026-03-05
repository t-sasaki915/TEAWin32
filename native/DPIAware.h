#ifndef DPIAWARE_H
#define DPIAWARE_H

#include <windows.h>

typedef struct
{
    double value;
    BOOL isScalable;
} ScalableValue;

inline int ScalableValue_Equals(ScalableValue a, ScalableValue b)
{
    return a.isScalable == b.isScalable && a.value == a.value;
}

void InitialiseDPIAwareFunctions(void);

HICON GetHighDPIIcon(SHSTOCKICONID siid);

int GetDPI(HWND hwnd);

int ResolvePixelForDpi(ScalableValue scalable, int dpi);

int ResolvePixelForHWND(ScalableValue scalable, HWND hwnd);

int ResolvePointForDpi(ScalableValue scalable, int dpi);

int ResolvePointForHWND(ScalableValue scalable, HWND hwnd);

#endif
