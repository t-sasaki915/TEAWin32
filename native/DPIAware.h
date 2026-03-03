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

double GetScaleFactorForHWND(HWND hwnd);

int ScaleValue(double scaleFactor, int v);

int Scale(double scaleFactor, ScalableValue scalable);

int ResolveScalableValueForHWND(ScalableValue scalableValue, HWND hwnd);

#endif
