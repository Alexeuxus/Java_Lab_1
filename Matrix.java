import java.util.Scanner;

class Complex {
    double re;
    double im;

    Complex(double re, double im) {
        this.re = re;
        this.im = im;
    }

    Complex add(Complex o) {
        return new Complex(re + o.re, im + o.im);
    }

    Complex sub(Complex o) {
        return new Complex(re - o.re, im - o.im);
    }

    Complex mul(Complex o) {
        return new Complex(re * o.re - im * o.im, re * o.im + im * o.re);
    }

    Complex div(Complex o) {
        double d = o.re * o.re + o.im * o.im;
        if (d == 0) {
            throw new ArithmeticException("деление на 0");
        }
        return new Complex((re * o.re + im * o.im) / d, (im * o.re - re * o.im) / d);
    }

    public String toString() {
        if (im < 0) {
            return re + "" + im + "i";
        }
        return re + "+" + im + "i";
    }
}

public class Matrix {
    Complex[][] a;
    int n;
    int m;

    public Matrix(int n, int m) {
        this.n = n;
        this.m = m;
        a = new Complex[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = new Complex(0, 0);
            }
        }
    }

    public Matrix add(Matrix b) {
        if (n != b.n || m != b.m) {
            throw new IllegalArgumentException("разный размер");
        }
        Matrix c = new Matrix(n, m);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                c.a[i][j] = a[i][j].add(b.a[i][j]);
            }
        }
        return c;
    }

    public Matrix mul(Matrix b) {
        if (m != b.n) {
            throw new IllegalArgumentException("нельзя умножить");
        }
        Matrix c = new Matrix(n, b.m);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < b.m; j++) {
                Complex s = new Complex(0, 0);
                for (int k = 0; k < m; k++) {
                    s = s.add(a[i][k].mul(b.a[k][j]));
                }
                c.a[i][j] = s;
            }
        }
        return c;
    }

    public Matrix div(Matrix b) {
        return mul(b.inv());
    }

    public Matrix transpose() {
        Matrix c = new Matrix(m, n);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                c.a[j][i] = a[i][j];
            }
        }
        return c;
    }

    public Complex det() {
        if (n != m) {
            throw new IllegalArgumentException("не квадратная");
        }
        Complex[][] b = new Complex[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                b[i][j] = a[i][j];
            }
        }
        Complex res = new Complex(1, 0);
        for (int k = 0; k < n; k++) {
            int p = k;
            for (int i = k + 1; i < n; i++) {
                double d1 = b[i][k].re * b[i][k].re + b[i][k].im * b[i][k].im;
                double d2 = b[p][k].re * b[p][k].re + b[p][k].im * b[p][k].im;
                if (d1 > d2) {
                    p = i;
                }
            }
            double dp = b[p][k].re * b[p][k].re + b[p][k].im * b[p][k].im;
            if (dp < 1e-12) {
                return new Complex(0, 0);
            }
            if (p != k) {
                Complex[] tmp = b[k];
                b[k] = b[p];
                b[p] = tmp;
                res = new Complex(-res.re, -res.im);
            }
            res = res.mul(b[k][k]);
            for (int i = k + 1; i < n; i++) {
                Complex f = b[i][k].div(b[k][k]);
                for (int j = k; j < n; j++) {
                    b[i][j] = b[i][j].sub(f.mul(b[k][j]));
                }
            }
        }
        return res;
    }

    public Matrix inv() {
        if (n != m) {
            throw new IllegalArgumentException("не квадратная");
        }
        Complex[][] b = new Complex[n][2 * n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                b[i][j] = a[i][j];
                if (i == j) {
                    b[i][j + n] = new Complex(1, 0);
                } else {
                    b[i][j + n] = new Complex(0, 0);
                }
            }
        }
        for (int k = 0; k < n; k++) {
            int p = k;
            for (int i = k + 1; i < n; i++) {
                double d1 = b[i][k].re * b[i][k].re + b[i][k].im * b[i][k].im;
                double d2 = b[p][k].re * b[p][k].re + b[p][k].im * b[p][k].im;
                if (d1 > d2) {
                    p = i;
                }
            }
            double dp = b[p][k].re * b[p][k].re + b[p][k].im * b[p][k].im;
            if (dp < 1e-12) {
                throw new ArithmeticException("нет обратной");
            }
            if (p != k) {
                Complex[] tmp = b[k];
                b[k] = b[p];
                b[p] = tmp;
            }
            Complex d = b[k][k];
            for (int j = 0; j < 2 * n; j++) {
                b[k][j] = b[k][j].div(d);
            }
            for (int i = 0; i < n; i++) {
                if (i == k) {
                    continue;
                }
                Complex f = b[i][k];
                for (int j = 0; j < 2 * n; j++) {
                    b[i][j] = b[i][j].sub(f.mul(b[k][j]));
                }
            }
        }
        Matrix c = new Matrix(n, n);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                c.a[i][j] = b[i][j + n];
            }
        }
        return c;
    }

    public void print() {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                System.out.print(a[i][j] + " ");
            }
            System.out.println();
        }
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        Matrix A = null;
        Matrix B = null;
        int cmd = -1;
        while (cmd != 0) {
            System.out.println("1 создать A");
            System.out.println("2 создать B");
            System.out.println("3 сложить");
            System.out.println("4 умножить");
            System.out.println("5 разделить");
            System.out.println("6 транспонировать A");
            System.out.println("7 определитель A");
            System.out.println("8 вывести");
            System.out.println("0 выход");
            cmd = sc.nextInt();
            try {
                if (cmd == 1) {
                    System.out.println("строки столбцы");
                    int n = sc.nextInt();
                    int m = sc.nextInt();
                    A = new Matrix(n, m);
                    System.out.println("re im");
                    for (int i = 0; i < n; i++) {
                        for (int j = 0; j < m; j++) {
                            double re = sc.nextDouble();
                            double im = sc.nextDouble();
                            A.a[i][j] = new Complex(re, im);
                        }
                    }
                } else if (cmd == 2) {
                    System.out.println("строки столбцы");
                    int n = sc.nextInt();
                    int m = sc.nextInt();
                    B = new Matrix(n, m);
                    System.out.println("re im");
                    for (int i = 0; i < n; i++) {
                        for (int j = 0; j < m; j++) {
                            double re = sc.nextDouble();
                            double im = sc.nextDouble();
                            B.a[i][j] = new Complex(re, im);
                        }
                    }
                } else if (cmd == 3) {
                    if (A == null || B == null) {
                        System.out.println("нет матрицы");
                    } else {
                        A.add(B).print();
                    }
                } else if (cmd == 4) {
                    if (A == null || B == null) {
                        System.out.println("нет матрицы");
                    } else {
                        A.mul(B).print();
                    }
                } else if (cmd == 5) {
                    if (A == null || B == null) {
                        System.out.println("нет матрицы");
                    } else {
                        A.div(B).print();
                    }
                } else if (cmd == 6) {
                    if (A == null) {
                        System.out.println("нет матрицы");
                    } else {
                        A.transpose().print();
                    }
                } else if (cmd == 7) {
                    if (A == null) {
                        System.out.println("нет матрицы");
                    } else {
                        System.out.println(A.det());
                    }
                } else if (cmd == 8) {
                    System.out.println("A");
                    if (A == null) {
                        System.out.println("нет");
                    } else {
                        A.print();
                    }
                    System.out.println("B");
                    if (B == null) {
                        System.out.println("нет");
                    } else {
                        B.print();
                    }
                }
            } catch (Exception e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
