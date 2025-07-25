//
// Created by jwscoggins on 6/7/21.
//
#include <cortex/IODevice.h>
#include <cortex/IAC.h>
#include <cortex/SystemCounter.h>
#include <cortex/builtins.h>
#include <newlib.h>
#include <iostream>
#include <cortex/ModernGCC.h>
#include <cortex/ChipsetInteract.h>
#include <stdio.h>
#include <math.h>
#include <string>
#include <sstream>
extern "C" {
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <microshell.h>
#include <math.h>
}
/* 'Uncomment' the line below to run   */
/* with 'register double' variables    */
/* defined, or compile with the        */
/* '-DROPT' option. Don't need this if */
/* registers used automatically, but   */
/* you might want to try it anyway.    */
/* #define ROPT */
namespace microshell {

    int read(ush_object*, char*ch) {
        uint16_t result = cortex::ChipsetBasicFunctions::Console::read();
        bool validResult = result != 0xFFFF;
        if (validResult) {
            *ch = result;
        }
        return validResult;
    }

    int write(ush_object*, char ch) {
        cortex::ChipsetBasicFunctions::Console::write(ch);
        return 1;
    }

    const ush_io_interface microshellIOInterface = {
        microshell::read,
        microshell::write,
    };
#define SHELL_BUFFER_SIZE 256
    char inputBuffer[SHELL_BUFFER_SIZE];
    char outputBuffer[SHELL_BUFFER_SIZE];
    const ush_descriptor microshellDescriptor = {
        &microshellIOInterface,
        inputBuffer, sizeof(inputBuffer),
        outputBuffer, sizeof(outputBuffer),
        SHELL_BUFFER_SIZE,
        "hitagimon960",
    };
    ush_object microshellObject;
    void setup();
    bool doMicroshell() {
        return ush_service(&microshellObject);
    }
}
void
init()
{
    // setup the chipset basic functions as one of the first things we actually do
    cortex::ChipsetBasicFunctions::begin();
    // make sure that we configured the c runtime to not buffer the inputs and outputs
    // this should allow each character to be printed out
    srandom(cortex::ChipsetBasicFunctions::Random::getHardwareRandomNumber());
    __builtin_i960_set_interrupt_control_reg(0xFCFDFEFF);
    cortex::clearSystemCounter();
    cortex::enableSystemCounter();
    
}
/**
 * @brief Compute the duration of execution and stash the result in a provided rusage type
 */
class DurationTimer {
public:
    DurationTimer(rusage& duration) : _duration(duration) {
        getrusage(RUSAGE_SELF, &_start);
    }
    ~DurationTimer() {
        rusage _end;
        getrusage(RUSAGE_SELF, &_end);
        _duration.ru_utime.tv_sec = _end.ru_utime.tv_sec - _start.ru_utime.tv_sec;
        _duration.ru_utime.tv_usec = _end.ru_utime.tv_usec - _start.ru_utime.tv_usec;
    }
private:
    rusage _start;
    rusage& _duration;
};
void
setup() {
    microshell::setup();
}
void loop() {
    microshell::doMicroshell();
}


/*--------------------- Start flops.c source code ----------------------*/

/*****************************/
/*          flops.c          */
/* Version 2.0,  18 Dec 1992 */
/*         Al Aburto         */
/*      aburto@nosc.mil      */
/*****************************/

/*
   Flops.c is a 'c' program which attempts to estimate your systems
   floating-point 'MFLOPS' rating for the FADD, FSUB, FMUL, and FDIV
   operations based on specific 'instruction mixes' (discussed below).
   The program provides an estimate of PEAK MFLOPS performance by making
   maximal use of register variables with minimal interaction with main
   memory. The execution loops are all small so that they will fit in
   any cache. Flops.c can be used along with Linpack and the Livermore
   kernels (which exersize memory much more extensively) to gain further
   insight into the limits of system performance. The flops.c execution
   modules also include various percent weightings of FDIV's (from 0% to
   25% FDIV's) so that the range of performance can be obtained when
   using FDIV's. FDIV's, being computationally more intensive than
   FADD's or FMUL's, can impact performance considerably on some systems.

   Flops.c consists of 8 independent modules (routines) which, except for
   module 2, conduct numerical integration of various functions. Module
   2, estimates the value of pi based upon the Maclaurin series expansion
   of atan(1). MFLOPS ratings are provided for each module, but the
   programs overall results are summerized by the MFLOPS(1), MFLOPS(2),
   MFLOPS(3), and MFLOPS(4) outputs.

   The MFLOPS(1) result is identical to the result provided by all
   previous versions of flops.c. It is based only upon the results from
   modules 2 and 3. Two problems surfaced in using MFLOPS(1). First, it
   was difficult to completely 'vectorize' the result due to the
   recurrence of the 's' variable in module 2. This problem is addressed
   in the MFLOPS(2) result which does not use module 2, but maintains
   nearly the same weighting of FDIV's (9.2%) as in MFLOPS(1) (9.6%).
   The second problem with MFLOPS(1) centers around the percentage of
   FDIV's (9.6%) which was viewed as too high for an important class of
   problems. This concern is addressed in the MFLOPS(3) result where NO
   FDIV's are conducted at all.

   The number of floating-point instructions per iteration (loop) is
   given below for each module executed:

   MODULE   FADD   FSUB   FMUL   FDIV   TOTAL  Comment
     1        7      0      6      1      14   7.1%  FDIV's
     2        3      2      1      1       7   difficult to vectorize.
     3        6      2      9      0      17   0.0%  FDIV's
     4        7      0      8      0      15   0.0%  FDIV's
     5       13      0     15      1      29   3.4%  FDIV's
     6       13      0     16      0      29   0.0%  FDIV's
     7        3      3      3      3      12   25.0% FDIV's
     8       13      0     17      0      30   0.0%  FDIV's

   A*2+3     21     12     14      5      52   A=5, MFLOPS(1), Same as
	   40.4%  23.1%  26.9%  9.6%          previous versions of the
						flops.c program. Includes
						only Modules 2 and 3, does
						9.6% FDIV's, and is not
						easily vectorizable.

   1+3+4     58     14     66     14     152   A=4, MFLOPS(2), New output
   +5+6+    38.2%  9.2%   43.4%  9.2%          does not include Module 2,
   A*7                                         but does 9.2% FDIV's.

   1+3+4     62      5     74      5     146   A=0, MFLOPS(3), New output
   +5+6+    42.9%  3.4%   50.7%  3.4%          does not include Module 2,
   7+8                                         but does 3.4% FDIV's.

   3+4+6     39      2     50      0      91   A=0, MFLOPS(4), New output
   +8       42.9%  2.2%   54.9%  0.0%          does not include Module 2,
						and does NO FDIV's.

   NOTE: Various timer routines are included as indicated below. The
	timer routines, with some comments, are attached at the end
	of the main program.

   NOTE: Please do not remove any of the printouts.

   EXAMPLE COMPILATION:
   UNIX based systems
       cc -DUNIX -O flops.c -o flops
       cc -DUNIX -DROPT flops.c -o flops
       cc -DUNIX -fast -O4 flops.c -o flops
       .
       .
       .
     etc.

   Al Aburto
   aburto@nosc.mil
*/
namespace FlopsCode {


/* Loops to run. Fixed at 15.0 seconds.*/

    
template<typename FloatType>
void
doFlops(const std::string& msg) {
    static const FloatType E2 = 0.48E-3;
    static const FloatType E3 = 0.411051E-6;
    static const FloatType D1 = 0.3999999946405E-1;
    static const FloatType D2 = 0.96E-3;
    static const FloatType D3 = 0.1233153E-5;
    static const FloatType B1 = -0.4999999999982;
    static const FloatType B2 = 0.4166666664651E-1;
    static const FloatType B3 = -0.1388888805755E-2;
    static const FloatType B4 = 0.24801428034E-4;
    static const FloatType B5 = -0.2754213324E-6;
    static const FloatType B6 = 0.20189405E-8;
    static const FloatType A0 = 1.0;
    static const FloatType A1 = -0.1666666666671334;
    static const FloatType A2 = 0.833333333809067E-2;
    FloatType A3 = 0.198412715551283E-3;
    FloatType A5 = 0.2507059876207E-7;
    static const FloatType A4 = 0.27557589750762E-5;
    static const FloatType A6 = 0.164105986683E-9;
    FloatType s, u, v, w, x;
    long m, n;
    FloatType TimeArray[3]; // variables needed for 'dtime()'
    FloatType T[36];                    /* Global Array used to hold timing    */
    /* results and other information.      */

    printf("\n");
    printf("   FLOPS C Program (%s), V2.0 18 Dec 1992\n\n", msg.c_str());

    /****************************/
    static const long loops = 15625;        /* Initial number of loops. */
    /*     DO NOT CHANGE!       */
    /****************************/

    /****************************************************/
    /* Set Variable Values.                             */
    /* T[1] references all timing results relative to   */
    /* one million loops.                               */
    /*                                                  */
    /* The program will execute from 31250 to 512000000 */
    /* loops based on a runtime of Module 1 of at least */
    /* TLimit = 15.0 seconds. That is, a runtime of 15  */
    /* seconds for Module 1 is used to determine the    */
    /* number of loops to execute.                      */
    /*                                                  */
    /* No more than NLimit = 512000000 loops are allowed*/
    /****************************************************/

    T[1] = 1.0E+06 / (FloatType) loops;

    static const FloatType TLimit = 15.0; // threshold to determine number
                                          // of loops to run. Fixed at 15.0
                                          // seconds
    static const long NLimit = 512000000;

    static const FloatType piref = 3.14159265358979324;
    static const FloatType one = 1.0;
    static const FloatType four = 4.0;
    static const FloatType five = 5.0;
    FloatType scale = one;
    printf("   Module     Error        RunTime      MFLOPS\n");
    printf("                            (usec)\n");
    /*************************/
    /* Initialize the timer. */
    /*************************/

    dtime(TimeArray);
    dtime(TimeArray);

    /*******************************************************/
    /* Module 1.  Calculate integral of df(x)/f(x) defined */
    /*            below.  Result is ln(f(1)). There are 14 */
    /*            FloatType precision operations per loop     */
    /*            ( 7 +, 0 -, 6 *, 1 / ) that are included */
    /*            in the timing.                           */
    /*            50.0% +, 00.0% -, 42.9% *, and 07.1% /   */
    /*******************************************************/
    n = loops;
    FloatType sa = 0.0;

    while (sa < TLimit) {
        n = 2 * n;
        x = one / (FloatType) n;                            /*********************/
        s = 0.0;                                        /*  Loop 1.          */
        v = 0.0;                                        /*********************/
        w = one;

        dtime(TimeArray);
        for (long i = 1; i <= n - 1; i++) {
            v = v + w;
            u = v * x;
            s = s + (D1 + u * (D2 + u * D3)) / (w + u * (D1 + u * (E2 + u * E3)));
        }
        dtime(TimeArray);
        sa = TimeArray[1];

        if (n == NLimit) break;
        //printf(" %10ld  %12.5lf\n",n,sa); 
    }

    scale = 1.0E+06 / (FloatType) n;
    T[1] = scale;

    /****************************************/
    /* Estimate nulltime ('for' loop time). */
    /****************************************/
    dtime(TimeArray);
    for (long i = 1; i <= n - 1; i++) {
    }
    dtime(TimeArray);
    FloatType nulltime = T[1] * TimeArray[1];
    if (nulltime < 0.0) nulltime = 0.0;

    T[2] = T[1] * sa - nulltime;

    sa = (D1 + D2 + D3) / (one + D1 + E2 + E3);
    FloatType sb = D1;

    T[3] = T[2] / 14.0;                             /*********************/
    sa = x * (sa + sb + 2.0 * s) / 2.0;           /* Module 1 Results. */
    sb = one / sa;                                  /*********************/
    n = (long) ((FloatType) (40000 * (long) sb) / scale);
    FloatType sc = sb - 25.2;
    T[4] = one / T[3];
    /********************/
    /*  DO NOT REMOVE   */
    /*  THIS PRINTOUT!  */
    /********************/
    printf("     1   %13.4le  %10.4lf  %10.4lf\n", sc, T[2], T[4]);

    m = n;

    /*******************************************************/
    /* Module 2.  Calculate value of PI from Taylor Series */
    /*            expansion of atan(1.0).  There are 7     */
    /*            FloatType precision operations per loop     */
    /*            ( 3 +, 2 -, 1 *, 1 / ) that are included */
    /*            in the timing.                           */
    /*            42.9% +, 28.6% -, 14.3% *, and 14.3% /   */
    /*******************************************************/

    s = -five;                                      /********************/
    sa = -one;                                       /* Loop 2.          */
    /********************/
    dtime(TimeArray);
    for (long i = 1; i <= m; i++) {
        s = -s;
        sa = sa + s;
    }
    dtime(TimeArray);
    T[5] = T[1] * TimeArray[1];
    if (T[5] < 0.0) T[5] = 0.0;

    sc = (FloatType) m;

    u = sa;                                         /*********************/
    v = 0.0;                                        /* Loop 3.           */
    w = 0.0;                                        /*********************/
    x = 0.0;

    dtime(TimeArray);
    for (long i = 1; i <= m; i++) {
        s = -s;
        sa = sa + s;
        u = u + 2.0;
        x = x + (s - u);
        v = v - s * u;
        w = w + s / u;
    }
    dtime(TimeArray);
    T[6] = T[1] * TimeArray[1];

    T[7] = (T[6] - T[5]) / 7.0;                   /*********************/
    m = (long) (sa * x / sc);                    /*  PI Results       */
    sa = four * w / five;                           /*********************/
    sb = sa + five / v;
    sc = 31.25;
    FloatType piprg = sb - sc / (v * v * v);
    FloatType pierr = piprg - piref;
    T[8] = one / T[7];
    /*********************/
    /*   DO NOT REMOVE   */
    /*   THIS PRINTOUT!  */
    /*********************/
    printf("     2   %13.4le  %10.4lf  %10.4lf\n", pierr, T[6] - T[5], T[8]);

    /*******************************************************/
    /* Module 3.  Calculate integral of sin(x) from 0.0 to */
    /*            PI/3.0 using Trapazoidal Method. Result  */
    /*            is 0.5. There are 17 FloatType precision    */
    /*            operations per loop (6 +, 2 -, 9 *, 0 /) */
    /*            included in the timing.                  */
    /*            35.3% +, 11.8% -, 52.9% *, and 00.0% /   */
    /*******************************************************/

    x = piref / (3.0 * (FloatType) m);              /*********************/
    s = 0.0;                                        /*  Loop 4.          */
    v = 0.0;                                        /*********************/

    dtime(TimeArray);
    for (long i = 1; i <= m - 1; i++) {
        v = v + one;
        u = v * x;
        w = u * u;
        s = s + u * ((((((A6 * w - A5) * w + A4) * w - A3) * w + A2) * w + A1) * w + one);
    }
    dtime(TimeArray);
    T[9] = T[1] * TimeArray[1] - nulltime;

    u = piref / 3.0;
    w = u * u;
    sa = u * ((((((A6 * w - A5) * w + A4) * w - A3) * w + A2) * w + A1) * w + one);

    T[10] = T[9] / 17.0;                            /*********************/
    sa = x * (sa + 2.0 * s) / 2.0;                /* sin(x) Results.   */
    sb = 0.5;                                       /*********************/
    sc = sa - sb;
    FloatType t11 = one / T[10];
    /*********************/
    /*   DO NOT REMOVE   */
    /*   THIS PRINTOUT!  */
    /*********************/
    printf("     3   %13.4le  %10.4lf  %10.4lf\n", sc, T[9], t11);

    /************************************************************/
    /* Module 4.  Calculate Integral of cos(x) from 0.0 to PI/3 */
    /*            using the Trapazoidal Method. Result is       */
    /*            sin(PI/3). There are 15 FloatType precision      */
    /*            operations per loop (7 +, 0 -, 8 *, and 0 / ) */
    /*            included in the timing.                       */
    /*            50.0% +, 00.0% -, 50.0% *, 00.0% /            */
    /************************************************************/
    A3 = -A3;
    A5 = -A5;
    x = piref / (3.0 * (FloatType) m);              /*********************/
    s = 0.0;                                        /*  Loop 5.          */
    v = 0.0;                                        /*********************/

    dtime(TimeArray);
    for (long i = 1; i <= m - 1; i++) {
        u = (FloatType) i * x;
        w = u * u;
        s = s + w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one;
    }
    dtime(TimeArray);
    T[12] = T[1] * TimeArray[1] - nulltime;

    u = piref / 3.0;
    w = u * u;
    sa = w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one;

    T[13] = T[12] / 15.0;                             /*******************/
    sa = x * (sa + one + 2.0 * s) / 2.0;            /* Module 4 Result */
    u = piref / 3.0;                               /*******************/
    w = u * u;
    sb = u * ((((((A6 * w + A5) * w + A4) * w + A3) * w + A2) * w + A1) * w + A0);
    sc = sa - sb;
    FloatType t14 = one / T[13];
    /*********************/
    /*   DO NOT REMOVE   */
    /*   THIS PRINTOUT!  */
    /*********************/
    printf("     4   %13.4le  %10.4lf  %10.4lf\n", sc, T[12], t14);

    /************************************************************/
    /* Module 5.  Calculate Integral of tan(x) from 0.0 to PI/3 */
    /*            using the Trapazoidal Method. Result is       */
    /*            ln(cos(PI/3)). There are 29 FloatType precision  */
    /*            operations per loop (13 +, 0 -, 15 *, and 1 /)*/
    /*            included in the timing.                       */
    /*            46.7% +, 00.0% -, 50.0% *, and 03.3% /        */
    /************************************************************/

    x = piref / (3.0 * (FloatType) m);              /*********************/
    s = 0.0;                                        /*  Loop 6.          */
    v = 0.0;                                        /*********************/

    dtime(TimeArray);
    for (long i = 1; i <= m - 1; i++) {
        u = (FloatType) i * x;
        w = u * u;
        v = u * ((((((A6 * w + A5) * w + A4) * w + A3) * w + A2) * w + A1) * w + one);
        s = s + v / (w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one);
    }
    dtime(TimeArray);
    T[15] = T[1] * TimeArray[1] - nulltime;

    u = piref / 3.0;
    w = u * u;
    sa = u * ((((((A6 * w + A5) * w + A4) * w + A3) * w + A2) * w + A1) * w + one);
    sb = w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one;
    sa = sa / sb;

    T[16] = T[15] / 29.0;                             /*******************/
    sa = x * (sa + 2.0 * s) / 2.0;                  /* Module 5 Result */
    sb = 0.6931471805599453;                          /*******************/
    sc = sa - sb;
    T[17] = one / T[16];
    /*********************/
    /*   DO NOT REMOVE   */
    /*   THIS PRINTOUT!  */
    /*********************/
    printf("     5   %13.4le  %10.4lf  %10.4lf\n", sc, T[15], T[17]);

    /************************************************************/
    /* Module 6.  Calculate Integral of sin(x)*cos(x) from 0.0  */
    /*            to PI/4 using the Trapazoidal Method. Result  */
    /*            is sin(PI/4)^2. There are 29 FloatType precision */
    /*            operations per loop (13 +, 0 -, 16 *, and 0 /)*/
    /*            included in the timing.                       */
    /*            46.7% +, 00.0% -, 53.3% *, and 00.0% /        */
    /************************************************************/

    x = piref / (four * (FloatType) m);               /*********************/
    s = 0.0;                                        /*  Loop 7.          */
    v = 0.0;                                        /*********************/

    dtime(TimeArray);
    for (long i = 1; i <= m - 1; i++) {
        u = (FloatType) i * x;
        w = u * u;
        v = u * ((((((A6 * w + A5) * w + A4) * w + A3) * w + A2) * w + A1) * w + one);
        s = s + v * (w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one);
    }
    dtime(TimeArray);
    T[18] = T[1] * TimeArray[1] - nulltime;

    u = piref / four;
    w = u * u;
    sa = u * ((((((A6 * w + A5) * w + A4) * w + A3) * w + A2) * w + A1) * w + one);
    sb = w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one;
    sa = sa * sb;

    T[19] = T[18] / 29.0;                             /*******************/
    sa = x * (sa + 2.0 * s) / 2.0;                  /* Module 6 Result */
    sb = 0.25;                                        /*******************/
    sc = sa - sb;
    T[20] = one / T[19];
    /*********************/
    /*   DO NOT REMOVE   */
    /*   THIS PRINTOUT!  */
    /*********************/
    printf("     6   %13.4le  %10.4lf  %10.4lf\n", sc, T[18], T[20]);


    /*******************************************************/
    /* Module 7.  Calculate value of the definite integral */
    /*            from 0 to sa of 1/(x+1), x/(x*x+1), and  */
    /*            x*x/(x*x*x+1) using the Trapizoidal Rule.*/
    /*            There are 12 FloatType precision operations */
    /*            per loop ( 3 +, 3 -, 3 *, and 3 / ) that */
    /*            are included in the timing.              */
    /*            25.0% +, 25.0% -, 25.0% *, and 25.0% /   */
    /*******************************************************/

    /*********************/
    s = 0.0;                                        /* Loop 8.           */
    w = one;                                        /*********************/
    sa = 102.3321513995275;
    v = sa / (FloatType) m;

    dtime(TimeArray);
    for (long i = 1; i <= m - 1; i++) {
        x = (FloatType) i * v;
        u = x * x;
        s = s - w / (x + w) - x / (u + w) - u / (x * u + w);
    }
    dtime(TimeArray);
    T[21] = T[1] * TimeArray[1] - nulltime;
    /*********************/
    /* Module 7 Results  */
    /*********************/
    T[22] = T[21] / 12.0;
    x = sa;
    u = x * x;
    sa = -w - w / (x + w) - x / (u + w) - u / (x * u + w);
    sa = 18.0 * v * (sa + 2.0 * s);

    m = -2000 * (long) sa;
    m = (long) ((FloatType) m / scale);

    sc = sa + 500.2;
    T[23] = one / T[22];
    /********************/
    /*  DO NOT REMOVE   */
    /*  THIS PRINTOUT!  */
    /********************/
    printf("     7   %13.4le  %10.4lf  %10.4lf\n", sc, T[21], T[23]);

    /************************************************************/
    /* Module 8.  Calculate Integral of sin(x)*cos(x)*cos(x)    */
    /*            from 0 to PI/3 using the Trapazoidal Method.  */
    /*            Result is (1-cos(PI/3)^3)/3. There are 30     */
    /*            FloatType precision operations per loop included */
    /*            in the timing:                                */
    /*               13 +,     0 -,    17 *          0 /        */
    /*            46.7% +, 00.0% -, 53.3% *, and 00.0% /        */
    /************************************************************/

    x = piref / (3.0* (FloatType) m);              /*********************/
    s = 0.0;                                        /*  Loop 9.          */
    v = 0.0;                                        /*********************/

    dtime(TimeArray);
    for (long i = 1; i <= m - 1; i++) {
        u = (FloatType) i * x;
        w = u * u;
        v = w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one;
        s = s + v * v * u * ((((((A6 * w + A5) * w + A4) * w + A3) * w + A2) * w + A1) * w + one);
    }
    dtime(TimeArray);
    FloatType t24 = T[1] * TimeArray[1] - nulltime;

    u = piref / 3.0;
    w = u * u;
    sa = u * ((((((A6 * w + A5) * w + A4) * w + A3) * w + A2) * w + A1) * w + one);
    sb = w * (w * (w * (w * (w * (B6 * w + B5) + B4) + B3) + B2) + B1) + one;
    sa = sa * sb * sb;

    T[25] = t24 / 30.0;                             /*******************/
    sa = x * (sa + 2.0 * s) / 2.0;                  /* Module 8 Result */
    sb = 0.29166666666666667;                         /*******************/
    sc = sa - sb;

    FloatType t26 = one / T[25];
    /*********************/
    /*   DO NOT REMOVE   */
    /*   THIS PRINTOUT!  */
    /*********************/
    printf("     8   %13.4le  %10.4lf  %10.4lf\n", sc, t24, t26);

    /**************************************************/
    /* MFLOPS(1) output. This is the same weighting   */
    /* used for all previous versions of the flops.c  */
    /* program. Includes Modules 2 and 3 only.        */
    /**************************************************/
    T[27] = (five * (T[6] - T[5]) + T[9]) / 52.0;
    T[28] = one / T[27];

    /**************************************************/
    /* MFLOPS(2) output. This output does not include */
    /* Module 2, but it still does 9.2% FDIV's.       */
    /**************************************************/
    T[29] = T[2] + T[9] + T[12] + T[15] + T[18];
    T[29] = (T[29] + four * T[21]) / 152.0;
    T[30] = one / T[29];

    /**************************************************/
    /* MFLOPS(3) output. This output does not include */
    /* Module 2, but it still does 3.4% FDIV's.       */
    /**************************************************/
    FloatType t31 = T[2] + T[9] + T[12] + T[15] + T[18];
    t31 = (t31+ T[21] + t24) / 146.0;
    FloatType t32 = one / t31;

    /**************************************************/
    /* MFLOPS(4) output. This output does not include */
    /* Module 2, and it does NO FDIV's.               */
    /**************************************************/
    FloatType t33 = (T[9] + T[12] + T[18] + t24) / 91.0;
    FloatType t34 = one / t33;


    printf("\n");
    printf("   Iterations      = %10ld\n", m);
    printf("   NullTime (usec) = %10.4lf\n", nulltime);
    printf("   MFLOPS(1)       = %10.4lf\n", T[28]);
    printf("   MFLOPS(2)       = %10.4lf\n", T[30]);
    printf("   MFLOPS(3)       = %10.4lf\n", t32);
    printf("   MFLOPS(4)       = %10.4lf\n\n", t34);
}

/*****************************************************/
/* Various timer routines.                           */
/* Al Aburto, aburto@nosc.mil, 18 Feb 1997           */
/*                                                   */
/* dtime(p) outputs the elapsed time seconds in p[1] */
/* from a call of dtime(p) to the next call of       */
/* dtime(p).  Use CAUTION as some of these routines  */
/* will mess up when timing across the hour mark!!!  */
/*                                                   */
/* For timing I use the 'user' time whenever         */
/* possible. Using 'user+sys' time is a separate     */
/* issue.                                            */
/*                                                   */
/* Example Usage:                                    */
/* [Timer options added here]                        */
/* FloatType RunTime, TimeArray[3];                     */
/* main()                                            */
/* {                                                 */
/* dtime(TimeArray);                                 */
/* [routine to time]                                 */
/* dtime(TimeArray);                                 */
/* RunTime = TimeArray[1];                           */
/* }                                                 */
/* [Timer code added here]                           */
/*****************************************************/

/******************************/
/* Timer code.                */
/******************************/

/*****************************************************/
/*  UNIX dtime(). This is the preferred UNIX timer.  */
/*  Provided by: Markku Kolkka, mk59200@cc.tut.fi    */
/*  HP-UX Addition by: Bo Thide', bt@irfu.se         */
/*****************************************************/
template<typename FloatType> 
void
dtime(FloatType ta[]) {
    struct rusage rusage;
    FloatType q = ta[2];

    getrusage(RUSAGE_SELF, &rusage);

    ta[2] = (FloatType) (rusage.ru_utime.tv_sec);
    ta[2] = ta[2] + (FloatType) (rusage.ru_utime.tv_usec) * 1.0e-06;
    ta[1] = ta[2] - q;
}


}
/*------ End flops.c code, say good night Jan! (Sep 1992) ------*/

namespace GraphicsOpcodes {
    enum Opcodes {
        Nothing = 0,
        DrawPixel,
        FillScreen,
        DrawLine,
        DrawFastVLine,
        DrawFastHLine,
        DrawRect,
        FillRect,
        DrawCircle,
        FillCircle,
        DrawTriangle,
        FillTriangle,
        DrawRoundRect,
        FillRoundRect,
    };
}
inline uint16_t color565(uint8_t r, uint8_t g, uint8_t b) noexcept {
    // taken from the Adafruit_GFX SPITFT implementation
    return (static_cast<uint16_t>(r & 0xF8) << 8) |
        (static_cast<uint16_t>(g & 0xFC) << 3) |
        (static_cast<uint16_t>(b) >> 3);
}
uint16_t randomColor() noexcept {
    uint32_t baseColor = rand();
    return color565(static_cast<uint8_t>(baseColor),
            static_cast<uint8_t>(baseColor >> 8),
            static_cast<uint8_t>(baseColor >> 16));
}

uint16_t screenWidth() noexcept { return cortex::ChipsetBasicFunctions::OLED::width(); }
uint16_t screenHeight() noexcept { return cortex::ChipsetBasicFunctions::OLED::height(); }

template<GraphicsOpcodes::Opcodes opcode>
void graphicsCommand(uint16_t arg0) noexcept { 
    cortex::ChipsetBasicFunctions::OLED::command(opcode, arg0); 
}
template<GraphicsOpcodes::Opcodes opcode>
void graphicsCommand(uint16_t arg0, uint16_t arg1) noexcept { 
    cortex::ChipsetBasicFunctions::OLED::command(opcode, arg0, arg1); 
}
template<GraphicsOpcodes::Opcodes opcode>
void graphicsCommand(uint16_t arg0, uint16_t arg1, uint16_t arg2) noexcept { 
    cortex::ChipsetBasicFunctions::OLED::command(opcode, arg0, arg1, arg2); 
}
template<GraphicsOpcodes::Opcodes opcode>
void graphicsCommand(uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3) noexcept { 
    cortex::ChipsetBasicFunctions::OLED::command(opcode, arg0, arg1, arg2, arg3); 
}
template<GraphicsOpcodes::Opcodes opcode>
void graphicsCommand(uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4) noexcept { 
    cortex::ChipsetBasicFunctions::OLED::command(opcode, arg0, arg1, arg2, arg3, arg4); 
}
template<GraphicsOpcodes::Opcodes opcode>
void graphicsCommand(uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5) noexcept { 
    cortex::ChipsetBasicFunctions::OLED::command(opcode, arg0, arg1, arg2, arg3, arg4, arg5); 
}
template<GraphicsOpcodes::Opcodes opcode>
void graphicsCommand(uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5, uint16_t arg6) noexcept { 
    cortex::ChipsetBasicFunctions::OLED::command(opcode, arg0, arg1, arg2, arg3, arg4, arg5, arg6); 
}
void fillScreen(uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::FillScreen>(color);
}
void fillScreen(uint8_t r, uint8_t g, uint8_t b) noexcept {
    fillScreen(color565(r, g, b));
}
void clearScreen() noexcept { fillScreen(0); }

void drawPixel(uint16_t x, uint16_t y, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawPixel>(x, y, color);
}
void drawLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawLine>(x0, y0, x1, y1, color);
}
void drawFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawFastVLine>(x, y, h, color);
}
void drawFastHLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawFastHLine>(x, y, h, color);
}
void drawRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawRect>(x, y, w, h, color);
}
void fillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::FillRect>(x, y, w, h, color);
}
void drawCircle(int16_t x0, int16_t y0, int16_t r, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawCircle>(x0, y0, r, color);
}
void fillCircle(int16_t x0, int16_t y0, int16_t r, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::FillCircle>(x0, y0, r, color);
}
void drawTriangle(int16_t x0, int16_t y0, int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawTriangle>(x0, y0, x1, y1, x2, y2, color);
}
void fillTriangle(int16_t x0, int16_t y0, int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::FillTriangle>(x0, y0, x1, y1, x2, y2, color);
}
void drawRoundRect(int16_t x0, int16_t y0, int16_t w, int16_t h, int16_t radius, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::DrawRoundRect>(x0, y0, w, h, radius, color);
}
void fillRoundRect(int16_t x0, int16_t y0, int16_t w, int16_t h, int16_t radius, uint16_t color) noexcept {
    graphicsCommand<GraphicsOpcodes::FillRoundRect>(x0, y0, w, h, radius, color);
}
uint32_t millis() noexcept { return cortex::ChipsetBasicFunctions::Timer::millis(); }
uint32_t micros() noexcept { return cortex::ChipsetBasicFunctions::Timer::micros(); }
void __attribute__((noinline)) delayMicroseconds(uint32_t wait) noexcept {
    volatile uint32_t end = wait + micros();
    while (end < micros()) { 

    }
}
void __attribute__((noinline)) delayMilliseconds(uint32_t wait) noexcept {
    volatile uint32_t end = wait + millis();
    while (end < millis()) { 

    }
}
void setRotation(uint8_t value) noexcept {
    cortex::ChipsetBasicFunctions::OLED::setRotation(value);
}

// graphicstest.ino functions
const uint16_t ColorBlack = color565(0, 0, 0);
const uint16_t ColorWhite = color565(255, 255, 255);
const uint16_t ColorRed = color565(255, 0, 0);
const uint16_t ColorGreen = color565(0, 255, 0);
const uint16_t ColorBlue = color565(0, 0, 255);
const uint16_t ColorMagenta = color565(255, 0, 255);
uint32_t testFillScreen() noexcept {
    uint32_t start = micros();
    fillScreen(ColorBlack);
    fillScreen(ColorRed);
    fillScreen(ColorGreen);
    fillScreen(ColorBlue);
    fillScreen(ColorBlack);
    return micros() - start;
}
uint32_t 
testLines(uint16_t color) noexcept {
    uint32_t start, t;
    int w = screenWidth(),
        h = screenHeight();
    clearScreen();
    int x1 = 0, 
        y1 = 0,
        y2 = h - 1,
        x2 = 0;
    start = micros();
    for (x2 = 0; x2 < w; x2 += 6) {
        drawLine(x1, y1, x2, y2, color);
    }
    x2 = w - 1;
    for (y2 = 0; y2 < h; y2 += 6) {
        drawLine(x1, y1, x2, y2, color);
    }
    t = micros() - start;
    clearScreen();
    x1 = w - 1;
    y1 = 0;
    y2 = h - 1;
    start = micros();
    for (x2 = 0; x2 < w; x2+=6) {
        drawLine(x1, y1, x2, y2, color);
    }
    x2 = 0;
    for (y2 = 0; y2 < h; y2 += 6) {
        drawLine(x1, y1, x2, y2, color);
    }
    t += micros() - start;
    clearScreen();
    x1 = 0;
    y1 = h - 1;
    y2 = 0;
    start = micros();
    for (x2 = 0; x2 < w; x2+=6) {
        drawLine(x1, y1, x2, y2, color);
    }
    x2 = w - 1;
    for (y2 = 0; y2 < h; y2 += 6) {
        drawLine(x1, y1, x2, y2, color);
    }
    t += micros() - start;
    clearScreen();
    x1 = w - 1;
    y1 = h - 1;
    y2 = 0;
    start = micros();
    for (x2 = 0; x2 < w; x2+=6) {
        drawLine(x1, y1, x2, y2, color);
    }
    x2 = 0;
    for (y2 = 0; y2 < h; y2 += 6) {
        drawLine(x1, y1, x2, y2, color);
    }
    t += micros() - start;
    return t;
}
uint32_t
testFastLines(uint16_t color1, uint16_t color2) noexcept {
    uint32_t start;
    int w = screenWidth(),
        h = screenHeight();
    clearScreen();
    start = micros();
    for (int y = 0; y < h; y += 5) {
        drawFastHLine(0, y, w, color1);
    }
    for (int x = 0; x < w; x += 5) {
        drawFastVLine(x, 0, h, color2);
    }
    return micros() - start;
}
uint32_t
testRects(uint16_t color) noexcept {
    uint32_t start;
    int cx = screenWidth() / 2,
        cy = screenHeight() / 2;
    clearScreen();
    int n = std::min(screenWidth(), screenHeight());
    start = micros();
    for (int i = 2; i < n; i += 6) {
        int i2 = i / 2;
        drawRect(cx-i2, cy-i2, i, i, color);
    }
    return micros() - start;
}
uint32_t
testFilledRects(uint16_t color1, uint16_t color2) noexcept {
    uint32_t start, t = 0;
    int cx = screenWidth() / 2 - 1,
        cy = screenHeight() / 2 - 1;
    clearScreen();
    int n = std::min(screenWidth(), screenHeight());
    for (int i = 2; i < n; i += 6) {
        int i2 = i / 2;
        start = micros();
        fillRect(cx-i2, cy-i2, i, i, color1);
        t += micros() - start;
        drawRect(cx-i2, cy-i2, i, i, color2);
    }
    return t;
}
uint32_t
testFilledCircles(uint8_t radius, uint16_t color) noexcept {
    int w = screenWidth(),
        h = screenHeight(),
        r2 = radius * 2;
    clearScreen();
    uint32_t start = micros();
    for (int x = radius; x < w; x += r2) {
        for (int y = radius; y < h; y += r2) {
            fillCircle(x, y, radius, color);
        }
    }
    return micros() - start;
}
uint32_t
testCircles(uint8_t radius, uint16_t color) {
    int r2 = radius * 2,
        w = screenWidth() + radius,
        h = screenHeight() + radius;
    uint32_t start = micros();
    for (int x = 0;x < w; x+=r2) {
        for (int y = 0; y < h; y+=r2) {
            drawCircle(x, y, radius, color);
        }
    }
    return micros() - start;
}

uint32_t
testTriangles() noexcept {
    int cx = screenWidth() / 2 - 1,
        cy = screenHeight() / 2 - 1;
    clearScreen();
    int n = std::min(cx, cy);
    uint32_t start = micros();
    for (int i = 0; i < n; i+=5) {
        drawTriangle(
                cx, cy - i, // peak
                cx - i, cy + i, // bottom left
                cx + i, cy + i, // bottom right
                color565(i, i, i));
    }
    return micros() - start;
}

uint32_t
testFilledTriangles() noexcept {
    int cx = screenWidth() / 2 - 1,
        cy = screenHeight() / 2 - 1;
    clearScreen();
    uint32_t t = 0;
    for (int i = std::min(cx, cy); i > 10; i-=5) {
        uint32_t start = micros();
        fillTriangle(cx, cy - i, cx - i, cy + i, cx + i, cy + i, color565(0, i * 10, i * 10));
        t += micros() - start;
        drawTriangle(
                cx, cy - i, // peak
                cx - i, cy + i, // bottom left
                cx + i, cy + i, // bottom right
                color565(i*10, i*10, 0));
    }
    return t;
}
uint32_t
testRoundRects() noexcept {
    int cx = screenWidth() / 2 - 1;
    int cy = screenHeight() / 2 - 1;
    clearScreen();
    int w = std::min(screenWidth(), screenHeight());
    uint32_t start = micros();
    for (int i = 0; i < w; i+=6) {
        int i2 = i / 2;
        drawRoundRect(cx - i2, cy - i2, i, i, i/8, color565(i,0,0));
    }
    return micros() - start;
}
uint32_t
testFilledRoundRects() noexcept {
    clearScreen();
    int cx = screenWidth() / 2 - 1;
    int cy = screenHeight() / 2 - 1;
    uint32_t start = micros();
    for (int i = std::min(screenWidth(), screenHeight()); i > 20; i-=6) {
        int i2 = i / 2;
        fillRoundRect(cx - i2, cy - i2, i, i, i / 8, color565(0, i, 0));
    }
    return micros() - start;
}
void
FizzleFade(int w, int h, uint16_t color) noexcept {
    // taken from fabien sanglards FizzleFade C example
    clearScreen();
    uint32_t rndval = 1;
    do {
        uint16_t y = rndval & 0x000FF; // Y = low 8 bits
        uint16_t x = (rndval & 0x1FF00) >> 8; // X = high 9 bits
        unsigned lsb = rndval & 1; // get the output bit
        rndval >>= 1; // shift register
        if (lsb) {
            rndval ^= 0x00012000;
        }
        if (x < w && y < h) {
            drawPixel(x, y, color);
        }
    } while (rndval != 1);
}


namespace microshell {
    void doGraphicsTest(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        printf("Running graphicstest\n");
        printf("Format: benchmark name:\ttime (usec)\n");
        delayMilliseconds(10);
        printf("Screen fill:\t%lu us\n", testFillScreen());
        delayMilliseconds(500);
        /// @todo add support for testText
        printf("Lines:\t%lu us\n", testLines(randomColor()));
        delayMilliseconds(500);
        printf("Horiz/Vert Lines:\t%lu us\n", testFastLines(randomColor(), randomColor()));
        delayMilliseconds(500);
        printf("Rectangles (outline):\t%lu us\n", testRects(randomColor()));
        delayMilliseconds(500);
        printf("Rectangles (filled):\t%lu us\n", testFilledRects(randomColor(), randomColor()));
        delayMilliseconds(500);
        printf("Circles (filled):\t%lu us\n", testFilledCircles(10, randomColor()));
        delayMilliseconds(500);
        printf("Circles (outline):\t%lu us\n", testCircles(10, randomColor()));
        delayMilliseconds(500);
        printf("Triangles (outline):\t%lu us\n", testTriangles());
        delayMilliseconds(500);
        printf("Triangles (filled):\t%lu us\n", testFilledTriangles());
        delayMilliseconds(500);
        printf("Rounded rects (outline):\t%lu us\n", testRoundRects());
        delayMilliseconds(500);
        printf("Rounded rects (filled):\t%lu us\n", testFilledRoundRects());
        delayMilliseconds(500);

        printf("Done!\n");
    }
    void doTestTriangles(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        testTriangles();
    }
    void doTestFilledTriangles(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        testFilledTriangles();
    }
    void doFilledCircleTest(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        testFilledCircles(10, randomColor());
    }
    void doCircleTest(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        testCircles(10, randomColor());
    }
    void doFillScreen(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        (void)testFillScreen();
    }
    void doTestRects(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        uint16_t color = randomColor();
        (void)testRects(color);
    }
    void doTestFilledRects(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        uint16_t color1 = randomColor();
        uint16_t color2 = randomColor();
        (void)testFilledRects(color1, color2);
    }
    void doTestFastLines(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        uint16_t color1 = randomColor();
        uint16_t color2 = randomColor();
        (void)testFastLines(color1, color2);
    }
    void doTestLines(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        uint16_t color = randomColor();
        (void)testLines(color);
    }
    void doFlops64Execution(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        FlopsCode::doFlops<double>("double precision");
    }
    void doFlops32Execution(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        FlopsCode::doFlops<float>("single precision");
    }
    void doFizzleFade(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        uint16_t w = std::min(screenWidth(), static_cast<uint16_t>(128));
        uint16_t h = std::min(screenHeight(), static_cast<uint16_t>(128));
        FizzleFade(w, h, randomColor());
    }
    void doScreenClear(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 4) {
            fillScreen(0);
            return;
        }
        uint64_t r = 0;
        uint64_t g = 0;
        uint64_t b = 0;
        if (sscanf(argv[1], "%llx", &r) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        if (sscanf(argv[2], "%llx", &g) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        if (sscanf(argv[3], "%llx", &b) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        // most of the precision will be lost
        fillScreen(r, g, b);
    }
    void doU64AddTest(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 3) {
           ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
           return;
        }
        uint64_t a = 0,
                 b = 0;
        if (sscanf(argv[1], "%llx", &a) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        if (sscanf(argv[2], "%llx", &b) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        uint64_t stockResult = a + b;
        uint64_t optionalResult = u64_add_via_addc(a, b);
        printf("Unsigned 64-bit numbers");
        printf("Operation: 0x%llx + 0x%llx\n", a, b);
        printf("Standard Result: 0x%llx\n", stockResult);
        printf("Addc Method: 0x%llx\n", optionalResult);
    }
    void doS64AddTest(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 3) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
            return;
        }
        int64_t a = 0;
        int64_t b = 0;
        if (sscanf(argv[1], "%lld", &a) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        if (sscanf(argv[2], "%lld", &b) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        int64_t stockResult = a + b;
        int64_t optionalResult = s64_add_via_addc(a, b);
        printf("Signed 64-bit numbers");
        printf("Operation: %lld + %lld\n", a, b);
        printf("Standard Result: %lld\n", stockResult);
        printf("Addc Method: %lld\n", optionalResult);
    }
    void doU64SubTest(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 3) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
            return;
        }
        uint64_t a = 0;
        uint64_t b = 0;
        if (sscanf(argv[1], "%llx", &a) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        if (sscanf(argv[2], "%llx", &b) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        uint64_t stockResult = a - b;
        uint64_t optionalResult = u64_subtract_via_subc(a, b);
        uint64_t optionalResult2 = u64_subtract_via_subc_v2(a, b);
        uint64_t optionalResult3 = u64_subtract_via_subc_v3(a, b);
        uint64_t optionalResult4 = u64_subtract_via_subc_v4(a, b);
        printf("Unsigned 64-bit numbers");
        printf("Operation: 0x%llx - 0x%llx\n", a, b);
        printf("Standard Result: 0x%llx\n", stockResult);
        printf("Subc Method (v1): 0x%llx\n", optionalResult);
        printf("Subc Method (v2): 0x%llx\n", optionalResult2);
        printf("Subc Method (v3): 0x%llx\n", optionalResult3);
        printf("Subc Method (v4): 0x%llx\n", optionalResult4);
        uint64_t optionalResult5 = u64_subtract_via_subc_v5(a, b);
        printf("Subc Method (v5): 0x%llx\n", optionalResult5);
        uint64_t optionalResult6 = u64_subtract_via_subc_v6(a, b);
        printf("Subc Method (v6): 0x%llx\n", optionalResult6);
        uint64_t optionalResult7 = u64_subtract_via_subc_v7(a, b);
        printf("Subc Method (v7): 0x%llx\n", optionalResult7);
    }
    void doS64SubTest(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 3) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
            return;
        }
        int64_t a = 0;
        int64_t b = 0;
        if (sscanf(argv[1], "%lld", &a) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        if (sscanf(argv[2], "%lld", &b) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        int64_t stockResult = a - b;
        int64_t optionalResult = s64_subtract_via_subc(a, b);
        int64_t optionalResult2 = s64_subtract_via_subc_v2(a, b);
        int64_t optionalResult3 = s64_subtract_via_subc_v3(a, b);
        int64_t optionalResult4 = s64_subtract_via_subc_v4(a, b);
        printf("Signed 64-bit numbers");
        printf("Operation: %lld - %lld\n", a, b);
        printf("Standard Result: %lld\n", stockResult);
#define X(var) ((var == stockResult) ? "yes" : "no")
        printf("Subc Method (v1): %lld [matches: %s]\n", optionalResult, X(optionalResult));
        printf("Subc Method (v2): %lld [matches: %s]\n", optionalResult2, X(optionalResult2));
        printf("Subc Method (v3): %lld [matches: %s]\n", optionalResult3, X(optionalResult3));
        printf("Subc Method (v4): %lld [matches: %s]\n", optionalResult4, X(optionalResult4));
        int64_t optionalResult5 = s64_subtract_via_subc_v5(a, b);
        printf("Subc Method (v5): %lld [matches: %s]\n", optionalResult5, X(optionalResult5));
        int64_t optionalResult6 = s64_subtract_via_subc_v6(a, b);
        printf("Subc Method (v6): %lld [matches: %s]\n", optionalResult6, X(optionalResult6));
        int64_t optionalResult7 = s64_subtract_via_subc_v7(a, b);
        printf("Subc Method (v7): %lld [matches: %s]\n", optionalResult7, X(optionalResult7));
#undef X
    }

    void doRotateOperation(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 3) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
            return;
        }
        uint32_t src = 0;
        uint32_t len = 0;
        if (sscanf(argv[1], "%lx", &src) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        if (sscanf(argv[2], "%lx", &len) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        uint32_t result = u32_rotate(src, len);
        printf("rotate(0x%lx, 0x%lx) = 0x%lx\n", src, len, result);
    }
    void doScanbitOperation(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 2) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
            return;
        }
        uint32_t src = 0;
        if (sscanf(argv[1], "%lx", &src) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        uint32_t result = scanbit32(src);
        printf("scanbit(0x%lx) = 0x%lx\n", src, result);
    }
    void doSpanbitOperation(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc != 2) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
            return;
        }
        uint32_t src = 0;
        if (sscanf(argv[1], "%lx", &src) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        uint32_t result = spanbit32(src);
        printf("spanbit(0x%lx) = 0x%lx\n", src, result);
    }
    inline bool isQuodigious(uint32_t value, uint32_t sum, uint32_t product)  {
        return ((value % product) == 0) && ((value % sum) == 0);
    }
    const uint32_t factors10[10] = {
        1, // 0
        10, // 1
        10 * 10, // 2
        10 * 10 * 10, // 3
        10 * 10 * 10 * 10, // 4
        10 * 10 * 10 * 10 * 10, // 5
        10 * 10 * 10 * 10 * 10 * 10, // 6
        10 * 10 * 10 * 10 * 10 * 10 * 10, // 7
        10 * 10 * 10 * 10 * 10 * 10 * 10 * 10, // 8
        10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10, // 9
    };
    void doQuodigious(uint8_t depth, uint32_t number = 0, uint32_t sum = 0, uint32_t product = 1) {
        switch (depth) {
            case 0:
                if (isQuodigious(number, sum, product)) {
                    printf("%lu\n", number);
                }
                break;
            case 1 ... 3: {
                const uint8_t innerDepth = depth - 1;
                const uint32_t baseFactor = factors10[innerDepth];
                for (int i = 2; i < 10; ++i) {
                    doQuodigious(innerDepth, number + (baseFactor * i), sum + i, product * i);
                }
                break;
            }
            default: {
                const uint8_t innerDepth = depth - 1;
                const uint32_t baseFactor = factors10[innerDepth];
                for (int i = 2; i < 10; ++i) {
                    // Skip fives if the depth is greater than 3 as they will never crop up
                    // This is based of actual observations
                    if (i == 5) {
                        continue;
                    }
                    doQuodigious(innerDepth, number + (baseFactor * i), sum + i, product * i);
                }
                break;
            }
        }
    }
    void quodigious_benchmark(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc == 1) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
        } else {
            for (int i = 1; i < argc; ++i) {
                uint32_t depth = 0;
                if (sscanf(argv[i], "%lu", &depth) == EOF) {
                    continue;
                } else if (depth == 0) {
                    continue;
                } else if (depth > 9) {
                    continue;
                }
                doQuodigious(static_cast<uint8_t>(depth));
            }
        }
    }
    void displayExecutionDuration(const rusage& duration) {
        std::cout << "execution duration: " << std::dec << duration.ru_utime.tv_sec << " sec" << std::endl;
        std::cout << "\t\t" << std::dec << duration.ru_utime.tv_usec << " usec" << std::endl;
    }
    struct ExecutionContainer {
        ExecutionContainer(const std::string& title) : _title(title) { }
        std::string _title;
        rusage _duration;
    };
    void runFullBenchmark(ush_object*, ush_file_descriptor const*, int, char* []) {
        ExecutionContainer components[13] = {
                ExecutionContainer("Entire Benchmark"),
                ExecutionContainer("FLOPS32"),
                ExecutionContainer("FLOPS64"),
                ExecutionContainer("Entire Quodigious"),
                ExecutionContainer("Quodigious 1 digit"),
                ExecutionContainer("Quodigious 2 digit"),
                ExecutionContainer("Quodigious 3 digit"),
                ExecutionContainer("Quodigious 4 digit"),
                ExecutionContainer("Quodigious 5 digit"),
                ExecutionContainer("Quodigious 6 digit"),
                ExecutionContainer("Quodigious 7 digit"),
                ExecutionContainer("Quodigious 8 digit"),
                ExecutionContainer("Quodigious 9 digit"),
        };
        {
            DurationTimer entire(components[0]._duration);
            printf("Running benchmarking suite\n");
            printf("Running flops32\n\n");
            {
                DurationTimer dt(components[1]._duration);
                FlopsCode::doFlops<float>("single precision");
            }
            printf("\nRunning flops64\n\n");
            {
                DurationTimer dt(components[2]._duration);
                FlopsCode::doFlops<double>("double precision");

                printf("\nRunning quodigious 1-9\n\n");
                {
                    DurationTimer dt(components[3]._duration);
                    {
                        DurationTimer q(components[4]._duration);
                        doQuodigious(1);
                    }
                    {
                        DurationTimer q(components[5]._duration);
                        doQuodigious(2);
                    }
                    {
                        DurationTimer q(components[6]._duration);
                        doQuodigious(3);
                    }
                    {
                        DurationTimer q(components[7]._duration);
                        doQuodigious(4);
                    }
                    {
                        DurationTimer q(components[8]._duration);
                        doQuodigious(5);
                    }
                    {
                        DurationTimer q(components[9]._duration);
                        doQuodigious(6);
                    }
                    {
                        DurationTimer q(components[10]._duration);
                        doQuodigious(7);
                    }
                    {
                        DurationTimer q(components[11]._duration);
                        doQuodigious(8);
                    }
                    {
                        DurationTimer q(components[12]._duration);
                        doQuodigious(9);
                    }
                }
            }
        }
        for (int i = 0; i < 13; ++i) {
            std::cout << components[i]._title << " execution duration: " << std::dec
                      << components[i]._duration.ru_utime.tv_sec << " seconds, " << std::dec
                      << components[i]._duration.ru_utime.tv_usec << " usec" << std::endl;
        }
    }
    size_t info_txt_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static bool initialized = false;
        static std::string message;
        if (!initialized) {
            std::stringstream stream;
            stream << "HITAGIMON" << std::endl
                   << "Built on " << __DATE__ << " at " << __TIME__ << std::endl
                   << "New Lib Version: " << _NEWLIB_VERSION << std::endl;
            message = stream.str();
        }
        *data = (uint8_t*)message.c_str();
        return message.length();
    }
    void benchmark_operation(ush_object* self, ush_file_descriptor const* file, int argc, char* argv[]) {
        if (argc == 1) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_WRONG_ARGUMENTS);
        } else {
            struct ush_file_descriptor const* targetCommand = ush_file_find_by_name(self, argv[1]);
            if (targetCommand == nullptr) {
                ush_print_status(self, USH_STATUS_ERROR_FILE_NOT_FOUND);
                return;
            }

            if (targetCommand->exec == nullptr) {
                ush_print_status(self, USH_STATUS_ERROR_FILE_NOT_EXECUTABLE);
                return;
            }
            // okay, argv bump by one and then attempt an execution
            uint32_t millisStart = cortex::ChipsetBasicFunctions::Timer::millis();
            {
                targetCommand->exec(self, targetCommand, argc - 1, &argv[1]);
            }
            uint32_t millisEnd = cortex::ChipsetBasicFunctions::Timer::millis();
            std::cout << std::endl << "Execution took: " << std::dec << (millisEnd - millisStart) << std::endl;
        }
    }
    ush_node_object cmd;
    const ush_file_descriptor cmdFiles[] = {
            {
                    "benchmark",
                    "See how long the given command takes to run",
                    "usage: benchmark command args",
                    benchmark_operation,
                    nullptr,
                    nullptr,
                    nullptr,
            },
    };
    ush_node_object fsroot;
    const ush_file_descriptor rootDesc[] = {
            {
                    "info.txt" ,
                    nullptr,
                    nullptr,
                    nullptr,
                    info_txt_get_data_callback,
                    nullptr,
                    nullptr,
            }
    };
    size_t millis_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char timeBuffer[16];
        unsigned long currentTime = cortex::ChipsetBasicFunctions::Timer::millis();
        snprintf(timeBuffer, sizeof(timeBuffer), "%lu\n", currentTime);
        timeBuffer[sizeof(timeBuffer) - 1] = 0;
        *data = (uint8_t*)timeBuffer;
        return strlen((char*)(*data));
    }
    size_t micros_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char timeBuffer[16];
        unsigned long currentTime = cortex::ChipsetBasicFunctions::Timer::micros();
        snprintf(timeBuffer, sizeof(timeBuffer), "%lu\n", currentTime);
        timeBuffer[sizeof(timeBuffer) - 1] = 0;
        *data = (uint8_t*)timeBuffer;
        return strlen((char*)(*data));
    }
    size_t unixtime_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char timeBuffer[16];
        unsigned long current = cortex::ChipsetBasicFunctions::Timer::unixtime();
        snprintf(timeBuffer, sizeof(timeBuffer), "%lu\n", current);
        timeBuffer[sizeof(timeBuffer) - 1] = 0;
        *data = (uint8_t*)timeBuffer;
        return strlen((char*)(*data));
    }
    size_t secondstime_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char timeBuffer[16];
        unsigned long current = cortex::ChipsetBasicFunctions::Timer::secondstime();
        snprintf(timeBuffer, sizeof(timeBuffer), "%lu\n", current);
        timeBuffer[sizeof(timeBuffer) - 1] = 0;
        *data = (uint8_t*)timeBuffer;
        return strlen((char*)(*data));
    }

    void isr_set_data_callback(struct ush_object* self, struct ush_file_descriptor const*, uint8_t* data, size_t size) {
        uint32_t value = 0;
        if (sscanf((const char*)data, "%lu", &value) == EOF) {
            ush_print_status(self, USH_STATUS_ERROR_COMMAND_SYNTAX_ERROR);
            return;
        }
        __builtin_i960_set_interrupt_control_reg(value);
    }
    size_t isr_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char buf[16];
        uint32_t curr = __builtin_i960_get_interrupt_control_reg();
        snprintf(buf, sizeof(buf), "%lu\n", curr);
        buf[sizeof(buf) - 1] = 0;
        *data = (uint8_t*)buf;
        return strlen((char*)(*data));
    }
    size_t systemcounter_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char buf[32];
        snprintf(buf, sizeof(buf), "%llu\n", cortex::getSystemCounter());
        buf[sizeof(buf) - 1] = 0;
        *data = (uint8_t*)buf;
        return strlen((char*)(*data));
    }
    size_t clk2_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char buf[32];
        snprintf(buf, sizeof(buf), "%lu\n", cortex::ChipsetBasicFunctions::Info::getChipsetClockSpeed());
        buf[sizeof(buf) - 1] = 0;
        *data = (uint8_t*)buf;
        return strlen((char*)(*data));
    }
    size_t clk1_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char buf[32];
        snprintf(buf, sizeof(buf), "%lu\n", cortex::ChipsetBasicFunctions::Info::getCPUClockSpeed());
        buf[sizeof(buf) - 1] = 0;
        *data = (uint8_t*)buf;
        return strlen((char*)(*data));
    }
    ush_node_object devNode;
    const ush_file_descriptor devDesc[] = {
        {
            "isr",
            nullptr,
            nullptr,
            nullptr,
            isr_get_data_callback,
            isr_set_data_callback,
            nullptr
        },
        {
            "millis",
            nullptr,
            nullptr,
            nullptr,
            millis_get_data_callback,
            nullptr,
            nullptr
        },
        {
            "micros",
            nullptr,
            nullptr,
            nullptr,
            micros_get_data_callback,
            nullptr,
            nullptr
        },
        {
            "unixtime",
            nullptr,
            nullptr,
            nullptr,
            unixtime_get_data_callback,
            nullptr,
            nullptr
        },
        {
            "secondstime",
            nullptr,
            nullptr,
            nullptr,
            secondstime_get_data_callback,
            nullptr,
            nullptr
        },
        {
            "system_counter",
            nullptr,
            nullptr,
            nullptr,
            systemcounter_get_data_callback,
            nullptr,
            nullptr
        }, 
        {
            "clk2",
            nullptr,
            nullptr,
            nullptr,
            clk2_get_data_callback,
            nullptr,
            nullptr
        },
        {
            "clk1",
            nullptr,
            nullptr,
            nullptr,
            clk1_get_data_callback,
            nullptr,
            nullptr
        },
    };
    size_t oled_width_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char buf[32];
        snprintf(buf, sizeof(buf), "%u\n", cortex::ChipsetBasicFunctions::OLED::width());
        buf[sizeof(buf) - 1] = 0;
        *data = (uint8_t*)buf;
        return strlen((char*)(*data));
    }
    size_t oled_height_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char buf[32];
        snprintf(buf, sizeof(buf), "%u\n", cortex::ChipsetBasicFunctions::OLED::height());
        buf[sizeof(buf) - 1] = 0;
        *data = (uint8_t*)buf;
        return strlen((char*)(*data));
    }
    size_t oled_rotation_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char buf[32];
        snprintf(buf, sizeof(buf), "%u\n", cortex::ChipsetBasicFunctions::OLED::getRotation());
        buf[sizeof(buf) - 1] = 0;
        *data = (uint8_t*)buf;
        return strlen((char*)(*data));
    }
    void oled_rotation_set_data_callback(struct ush_object*, struct ush_file_descriptor const*, uint8_t* data, size_t size) {
        if (size > 0) {
            cortex::ChipsetBasicFunctions::OLED::setRotation(data[0]);
        }
    }
    ush_node_object oledNode;
    const ush_file_descriptor oledDesc[] = {
        {
            "width",
            nullptr,
            nullptr,
            nullptr,
            oled_width_get_data_callback,
            nullptr,
            nullptr,
        },
        {
            "height",
            nullptr,
            nullptr,
            nullptr,
            oled_height_get_data_callback,
            nullptr,
            nullptr,
        },
        {
            "rotation",
            nullptr,
            nullptr,
            nullptr,
            oled_rotation_get_data_callback,
            oled_rotation_set_data_callback,
            nullptr,
        },
    };
    size_t eeprom_capacity_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char timeBuffer[16];
        /// @todo reimplement through memory mapped io
        unsigned long capacity = cortex::EEPROM().capacity();
        snprintf(timeBuffer, sizeof(timeBuffer), "%lu\n", capacity);
        timeBuffer[sizeof(timeBuffer) - 1] = 0;
        *data = (uint8_t*)timeBuffer;
        return strlen((char*)(*data));
    }
    size_t eeprom_data_get_data_callback(struct ush_object*, struct ush_file_descriptor const*, uint8_t** data) {
        *data = cortex::EEPROM().data();
        return cortex::EEPROM().capacity();
    }
    void eeprom_data_set_data_callback(struct ush_object*, struct ush_file_descriptor const*, uint8_t* data, size_t size) {
        memcpy(cortex::EEPROM().data(), data, size);
    }
    ush_node_object eepromRoot;
    const ush_file_descriptor eepromDesc[] = {
            {
                    "capacity",
                    nullptr,
                    nullptr,
                    nullptr,
                    eeprom_capacity_get_data_callback,
                    nullptr,
                    nullptr
            },
            {
                "block",
                nullptr,
                nullptr,
                nullptr,
                eeprom_data_get_data_callback,
                eeprom_data_set_data_callback,
                nullptr,
            },
            /// @todo finish implementing
    };
    size_t sram_capacity_get_data_callback(struct ush_object* self, struct ush_file_descriptor const* file, uint8_t** data) {
        static char timeBuffer[16];
        unsigned long capacity = cortex::SRAM().capacity();
        snprintf(timeBuffer, sizeof(timeBuffer), "%lu\n", capacity);
        timeBuffer[sizeof(timeBuffer) - 1] = 0;
        *data = (uint8_t*)timeBuffer;
        return strlen((char*)(*data));
    }
    size_t sram_data_get_data_callback(struct ush_object*, struct ush_file_descriptor const*, uint8_t** data) {
        *data = cortex::SRAM().data();
        return cortex::SRAM().capacity();
    }
    void sram_data_set_data_callback(struct ush_object*, struct ush_file_descriptor const*, uint8_t* data, size_t size) {
        memcpy(cortex::SRAM().data(), data, size);
    }
    ush_node_object sramRoot;
    const ush_file_descriptor sramDesc[] = {
            {
                    "capacity",
                    nullptr,
                    nullptr,
                    nullptr,
                    sram_capacity_get_data_callback,
                    nullptr,
                    nullptr
            },
            {
                    "block",
                    nullptr,
                    nullptr,
                    nullptr,
                    sram_data_get_data_callback,
                    sram_data_set_data_callback,
                    nullptr,
            },
    };
    ush_node_object binNode;
    const ush_file_descriptor binDesc[] = {
        {
            "quodigious",
            "Run the 32-bit linear quodigious benchmark",
            "usage: quodigious {0-9}+\n",
            quodigious_benchmark,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "spanbit",
            "invoke the spanbit instruction" ,
            "usage: spanbit value\n",
            doSpanbitOperation, // exec
            nullptr, // get_data
            nullptr, // set_data
            nullptr, // process
        },
        {
            "scanbit",
            "invoke the scanbit instruction" ,
            "usage: scanbit value\n", // help
            doScanbitOperation, // exec
            nullptr, // get_data
            nullptr, // set_data
            nullptr, // process
        },
        {
            "rotate",
            "inspect the results of the rotate instruction",
            "usage: rotate src len",
            doRotateOperation,
            nullptr,
            nullptr,
            nullptr,

        },
        {
            "addc_u64",
            "compare results of different ways to add 64-bit unsigned values",
            "usage: addc_u64 src1 src2",
            doU64AddTest,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "addc_s64",
            "compare results of different ways to add 64-bit signed values",
            "usage: addc_s64 src1 src2",
            doS64AddTest,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "subc_u64",
            "compare results of different ways to subtract 64-bit unsigned values",
            "usage: subc_u64 src1 src2",
            doU64SubTest,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "subc_s64",
            "compare results of different ways to subtract 64-bit signed values",
            "usage: subc_s64 src1 src2",
            doS64SubTest,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "flops64",
            "run flops double precision benchmark" ,
            nullptr, // help
            doFlops64Execution, // exec
            nullptr, // get_data
            nullptr, // set_data
            nullptr, // process
        },
        {
            "flops32",
            "run flops single precision benchmark" ,
            nullptr, // help
            doFlops32Execution, // exec
            nullptr, // get_data
            nullptr, // set_data
            nullptr, // process
        },
        {
            "run_benchmark",
            "run a series of benchmarks",
            nullptr,
            runFullBenchmark,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "fizzle_fade",
            "do the Wolf3d FizzleFade sequence on the oled",
            nullptr, // help
            doFizzleFade, // exec
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "fill_screen_test",
            "Do Adafruit_GFX style fill screen test",
            nullptr, // help
            doFillScreen, // exec
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "clear_screen",
            "clear oled screen",
            "usage: clear_screen [r g b]", // help
            doScreenClear, // exec
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "lines_test",
            "Run testLiens from graphicstest.ino",
            nullptr,
            doTestLines, // exec
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "fast_lines_test",
            "Run testFastLines from graphicstest.ino",
            nullptr,
            doTestFastLines, // exec
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "rects_test",
            "Run testRects from graphicstest.ino",
            nullptr,
            doTestRects, // exec
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "filled_rects_test",
            "Run testFilledRects from graphicstest.ino",
            nullptr,
            doTestRects, // exec
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "circles_test",
            "Run testCircles(10, randomColor()) from graphicstest.ino",
            nullptr,
            doCircleTest,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "filled_circles_test",
            "Run testFilledCircles(10, randomColor()) from graphicstest.ino",
            nullptr,
            doFilledCircleTest,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "triangles_test",
            "Run testTriangles from graphicstest.ino",
            nullptr,
            doTestTriangles,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "filled_triangles_test",
            "Run testFilledTriangles from graphicstest.ino",
            nullptr,
            doTestFilledTriangles,
            nullptr,
            nullptr,
            nullptr,
        },
        {
            "graphicstest.ino",
            "Run a simple benchmark tool based off of graphicstest.ino",
            nullptr,
            doGraphicsTest,
            nullptr,
            nullptr,
            nullptr,
        },
    };
    void
    setup() {
        ush_init(&microshellObject, &microshellDescriptor);
        ush_commands_add(&microshellObject, &cmd, cmdFiles, sizeof(cmdFiles) / sizeof(cmdFiles[0]));
        ush_node_mount(&microshellObject, "/", &fsroot, rootDesc, sizeof(rootDesc) / sizeof(rootDesc[0]));
        ush_node_mount(&microshellObject, "/bin", &binNode, binDesc, sizeof(binDesc)/sizeof(binDesc[0]));
        ush_node_mount(&microshellObject, "/dev", &devNode, devDesc, sizeof(devDesc)/sizeof(devDesc[0]));
        ush_node_mount(&microshellObject, "/dev/eeprom", &eepromRoot, eepromDesc, sizeof(eepromDesc) / sizeof(eepromDesc[0]));
        ush_node_mount(&microshellObject, "/dev/sram", &sramRoot, sramDesc, sizeof(sramDesc) / sizeof(sramDesc[0]));
        ush_node_mount(&microshellObject, "/dev/oled", &oledNode, oledDesc, sizeof(oledDesc) / sizeof(oledDesc[0]));

    }
}

int main(void) {
    init();
    setup();
    for (;; ) {
        loop();
    }
    return 0;
}
