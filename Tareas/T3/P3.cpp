#include <iostream>
using namespace std;


long naive_f (long n)
{
    if (n == 0)
    {
        return 0;
    }
    else
    {
        short c = (n % 2 == 0) ? 1 : -1;
        return naive_f(n - 1) + c * n;
    }
}

// PARTE a)
// ¿Qué valor observó que “agota” la pila?
// 180000 (no es el minimo, pero en 170000 no se agota)


// PARTE b)
long smart_f_aux (long cn, long n, long c)
{
    if (n == 0)
    {
        return cn;
    }
    else
    {
        return smart_f_aux(cn + (n * c), n - 1, c * -1);
    }
}

long smart_f (long n)
{
    return smart_f_aux(0, n, ((n % 2 == 0) ? 1 : -1));
}


int main ()
{
    long number, res;
    cout << "Please enter a natural number: ";
    cin >> number;
    //res = naive_f(number);
    res = smart_f(number);
    cout << "Its image through f is: " << res << ".\n";
    return 0;
}
