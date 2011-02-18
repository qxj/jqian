//
// Description:
// A simple unit-test framework which aims to testing simple programs like utility class, algorithm...
//
// How to use:
// You only need to know 3 macros to use this framework: TESTCASE, ASSERT_TRUE, RUN_ALL_CASES
// TESTCASE(testname)
// {
//     ASSERT_TRUE(1 + 1 ==  2);
//     return true;
// }
// ...
// RUN_ALL_CASES();
//
// Author: lzprgmr
// Date: 1/8/2011
//

#pragma once

#include <map>
#include <iostream>

#if defined(_WIN32)
#include <Windows.h>
#endif

#if !defined(LazyTestOut)
#define LazyTestOut std::cout
#endif

// typedefs
typedef unsigned int uint32_t;
typedef bool (*TestFunc) ();
typedef std::map<const char*, TestFunc> TestCaseMap;

// Manage and run all test cases
class TestMgr{
public:
    static TestMgr* Get(){
        static TestMgr _instance;
        return &_instance;
    }

    void AddTest(const char* tcName, TestFunc tcFunc){
        m_tcList[tcName] = tcFunc;
    }

    uint32_t RunAllCases(){
        uint32_t failure = 0;
        for(TestCaseMap::iterator it = m_tcList.begin();
            it != m_tcList.end(); ++it){
            LazyTestOut << "Running " << it->first << "... " << std::endl;
            bool bRes = RunCase(it->second);
            if(bRes) LazyTestOut << "\tPass" << std::endl;
            else failure++;
        }
        LazyTestOut << "\n" << "Totally "<< failure << " cases failed!!!" << std::endl;
        return failure;
    }

private:
    bool RunCase(TestFunc tf){
        bool bRes = false;
#if defined(_WIN32)
        // Windows use SEH to handle machine exceptions
        __try{
            bRes = tf();
        }__except(EXCEPTION_EXECUTE_HANDLER){
            LazyTestOut << "\tException caught!" << std::endl;
            bRes = false;
        }
#else
        //Non-Windows OS that doesn't support SEH - the singal mechanism (SIGSEGV) can't work well as SEH to handle the problem
        bRes = tf();
#endif
        return bRes;
    }

private:
    TestCaseMap m_tcList;
};

// Register a test case
class TestCaseRegister{
public:
    TestCaseRegister(const char* tcName, TestFunc tcFunc) {
        TestMgr::Get()->AddTest(tcName, tcFunc);
    }
};


// To use this test framework, you only need to know 3 macros:
#define TESTCASE(tc)                                        \
    class class_##tc{                                       \
    public:                                                 \
    static bool tc(){                                       \
        _result = true;                                     \
        run();                                              \
        return _result;                                     \
    }                                                       \
    static void run();                                      \
    private:                                                \
    static bool _result;                                    \
    };                                                      \
    bool class_##tc::_result = true;                        \
    TestCaseRegister register_##tc(#tc, class_##tc::tc);    \
    void class_##tc::run()

#define ASSERT_TRUE(expr) do {                                          \
        if(!(expr)) {                                                   \
            LazyTestOut << "\tFailed at: " << __FILE__ << ": Line " <<__LINE__ << std::endl; \
            LazyTestOut << "\tExpression: " << #expr << std::endl;      \
            _result = false; return;}} while(false)

#define RUN_ALL_CASES()  do {TestMgr::Get()->RunAllCases(); } while(false)
