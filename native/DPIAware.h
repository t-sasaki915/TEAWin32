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

BOOL InitialiseDPIAwareFunctions(void);

HICON GetHighDPIIcon(SHSTOCKICONID siid);

int GetDPI(HWND hwnd);

BOOL GetCachedDpi(HWND hwnd, int *resultPtr);

int ResolvePixel(ScalableValue scalable, int dpi);

int ResolvePoint(ScalableValue scalable, int dpi);

#endif
