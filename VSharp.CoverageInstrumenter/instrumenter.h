#ifndef INSTRUMENTER_H_
#define INSTRUMENTER_H_

#include "ILRewriter.h"

#include <set>
#include <map>

#ifdef IMAGEHANDLER_EXPORTS
#define IMAGEHANDLER_API __declspec(dllexport)
#else
#define IMAGEHANDLER_API __declspec(dllimport)
#endif

extern "C" IMAGEHANDLER_API void SetEntryMain(char* assemblyName, int assemblyNameLength, char* moduleName, int moduleNameLength, int methodToken);
extern "C" IMAGEHANDLER_API void GetHistory(UINT_PTR size, UINT_PTR bytes);

namespace vsharp {

extern WCHAR* mainAssemblyName;
extern int mainAssemblyNameLength;
extern WCHAR* mainModuleName;
extern int mainModuleNameLength;
extern mdMethodDef mainToken;
extern bool rewriteMainOnly;

extern std::set<std::pair<FunctionID, ModuleID>> instrumentedMethods;

class Instrumenter {
private:
    ICorProfilerInfo8 &m_profilerInfo;  // Does not have ownership

    mdMethodDef m_jittedToken;
    ModuleID m_moduleId;

    char *m_signatureTokens;
    unsigned m_signatureTokensLength;

    HRESULT doInstrumentation(ModuleID oldModuleId, int methodId, const WCHAR *moduleName, ULONG moduleNameLength);

    bool currentMethodIsMain(const WCHAR *moduleName, int moduleSize, mdMethodDef method) const;

public:
    explicit Instrumenter(ICorProfilerInfo8 &profilerInfo);
    ~Instrumenter();

    HRESULT instrument(FunctionID functionId);
};

}

#endif // INSTRUMENTER_H_
