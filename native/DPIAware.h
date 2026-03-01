#ifndef DPIAWARE_H
#define DPIAWARE_H

#include <windows.h>

typedef struct
{
    double value;
    BOOL isScalable;
} CScalableValue;

inline int CScalableValue_Equals(CScalableValue a, CScalableValue b)
{
    return a.isScalable == b.isScalable && a.value == a.value;
}

void InitialiseDPIAwareFunctions(void);

HICON GetHighDPIIcon(SHSTOCKICONID siid);

double GetScaleFactorForHWND(HWND hwnd);

int ScaleValue(double scaleFactor, int v);

int Scale(double scaleFactor, CScalableValue scalable);

int ResolveScalableValueForHWND(CScalableValue scalableValue, HWND hwnd);

#endif
