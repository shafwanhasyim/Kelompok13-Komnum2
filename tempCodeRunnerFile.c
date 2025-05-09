/**
 * Program untuk melengkapi data yang hilang pada tahun 2005, 2006, 2015, dan 2016
 * menggunakan metode pencocokan kurva polinomial.
 * 
 * Metode ini menggunakan:
 * - Regresi polinomial (y = a_0 + a_1*x + a_2*x^2 + ... + a_n*x^n) untuk data persentase pengguna internet
 * - Regresi polinomial juga untuk data populasi
 * 
 * Pemrosesan data mengikuti pola yang sama dengan kode original untuk konsistensi.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MAX_ROWS 100
#define MAX_YEARS 100
#define MISSING_YEARS 4
#define MAX_DEGREE 3  // Derajat maksimum untuk polinomial

// Struktur untuk menyimpan data dari file CSV
typedef struct {
    int year;
    double percentage;
    double population;
} DataRow;

// Fungsi untuk membaca data dari file CSV
int readCSV(const char* filename, DataRow data[], int* years) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error: Tidak dapat membuka file %s\n", filename);
        return 0;
    }

    char line[256];
    int rows = 0;
    
    // Baca header
    fgets(line, sizeof(line), file);
    
    // Baca data
    while (fgets(line, sizeof(line), file) && rows < MAX_ROWS) {
        char* token = strtok(line, ",");
        if (token != NULL) {
            data[rows].year = atoi(token);
            years[rows] = data[rows].year;
        }
        
        token = strtok(NULL, ",");
        if (token != NULL) {
            data[rows].percentage = atof(token);
        }
        
        token = strtok(NULL, ",");
        if (token != NULL) {
            data[rows].population = atof(token);
        }
        
        rows++;
    }
    
    fclose(file);
    return rows;
}

// Fungsi untuk menulis data ke file CSV
void writeCSV(const char* filename, DataRow data[], int rows) {
    FILE* file = fopen(filename, "w");
    if (file == NULL) {
        printf("Error: Tidak dapat membuka file %s untuk ditulis\n", filename);
        return;
    }
    
    // Tulis header
    fprintf(file, "Year,Percentage_Internet_User,Population\n");
    
    // Tulis data
    for (int i = 0; i < rows; i++) {
        fprintf(file, "%d,%.6f,%.0f\n", data[i].year, data[i].percentage, data[i].population);
    }
    
    fclose(file);
}

// Fungsi untuk membandingkan dua DataRow berdasarkan tahun
int compareDataRows(const void* a, const void* b) {
    DataRow* dataA = (DataRow*)a;
    DataRow* dataB = (DataRow*)b;
    return dataA->year - dataB->year;
}

// Fungsi untuk membandingkan dua angka (digunakan untuk qsort)
int compare(const void* a, const void* b) {
    return (*(int*)a - *(int*)b);
}

// Fungsi untuk memeriksa apakah tahun tertentu ada dalam data
int yearExists(int year, int years[], int count) {
    for (int i = 0; i < count; i++) {
        if (years[i] == year) {
            return 1;
        }
    }
    return 0;
}

// Alokasi matriks
double** allocateMatrix(int rows, int cols) {
    double** matrix = (double**)malloc(rows * sizeof(double*));
    for (int i = 0; i < rows; i++) {
        matrix[i] = (double*)malloc(cols * sizeof(double));
    }
    return matrix;
}

// Dealokasi matriks
void freeMatrix(double** matrix, int rows) {
    for (int i = 0; i < rows; i++) {
        free(matrix[i]);
    }
    free(matrix);
}

// Eliminasi Gauss-Jordan untuk penyelesaian sistem persamaan linier
void gaussJordan(double** A, double* B, double* X, int n) {
    // Matriks augmented [A|B]
    double** augmented = allocateMatrix(n, n + 1);
    
    // Inisialisasi matriks augmented
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            augmented[i][j] = A[i][j];
        }
        augmented[i][n] = B[i];
    }
    
    // Proses eliminasi
    for (int i = 0; i < n; i++) {
        // Cari pivot maksimum di kolom saat ini
        int max_row = i;
        for (int j = i + 1; j < n; j++) {
            if (fabs(augmented[j][i]) > fabs(augmented[max_row][i])) {
                max_row = j;
            }
        }
        
        // Tukar baris jika perlu
        if (max_row != i) {
            for (int j = 0; j <= n; j++) {
                double temp = augmented[i][j];
                augmented[i][j] = augmented[max_row][j];
                augmented[max_row][j] = temp;
            }
        }
        
        // Normalisasi baris pivot
        double pivot = augmented[i][i];
        if (fabs(pivot) < 1e-10) {
            printf("Error: Matriks singular, tidak dapat menyelesaikan sistem.\n");
            exit(1);
        }
        
        for (int j = 0; j <= n; j++) {
            augmented[i][j] /= pivot;
        }
        
        // Eliminasi baris lain
        for (int j = 0; j < n; j++) {
            if (j != i) {
                double factor = augmented[j][i];
                for (int k = 0; k <= n; k++) {
                    augmented[j][k] -= factor * augmented[i][k];
                }
            }
        }
    }
    
    // Ambil solusi
    for (int i = 0; i < n; i++) {
        X[i] = augmented[i][n];
    }
    
    // Bersihkan memori
    freeMatrix(augmented, n);
}

// Fungsi untuk melakukan regresi polinomial
void polynomialRegression(double x[], double y[], int n, double coef[], int degree) {
    // Membuat matriks normal equations
    double** A = allocateMatrix(degree + 1, degree + 1);
    double* B = (double*)malloc((degree + 1) * sizeof(double));
    
    // Inisialisasi matriks A dan vektor B dengan 0
    for (int i = 0; i <= degree; i++) {
        for (int j = 0; j <= degree; j++) {
            A[i][j] = 0;
        }
        B[i] = 0;
    }
    
    // Hitung komponen matriks A
    for (int i = 0; i <= degree; i++) {
        for (int j = 0; j <= degree; j++) {
            for (int k = 0; k < n; k++) {
                A[i][j] += pow(x[k], i + j);
            }
        }
    }
    
    // Hitung komponen vektor B
    for (int i = 0; i <= degree; i++) {
        for (int k = 0; k < n; k++) {
            B[i] += y[k] * pow(x[k], i);
        }
    }
    
    // Selesaikan sistem persamaan linier untuk mendapatkan koefisien
    gaussJordan(A, B, coef, degree + 1);
    
    // Bersihkan memori
    freeMatrix(A, degree + 1);
    free(B);
}

// Fungsi untuk mengevaluasi polinomial pada nilai x tertentu
double evaluatePolynomial(double x, double coef[], int degree) {
    double result = 0;
    for (int i = 0; i <= degree; i++) {
        result += coef[i] * pow(x, i);
    }
    return result;
}

// Fungsi untuk normalisasi tahun (mengurangi dengan tahun dasar)
void normalizeYears(int original_years[], double normalized_years[], int n, int base_year) {
    for (int i = 0; i < n; i++) {
        normalized_years[i] = (double)(original_years[i] - base_year);
    }
}

int main() {
    DataRow data[MAX_ROWS];
    int years[MAX_YEARS];
    int rows = readCSV("Data Tugas Pemrograman A.csv", data, years);
    
    if (rows == 0) {
        printf("Error: Tidak ada data yang dibaca!\n");
        return 1;
    }
    
    printf("Data yang berhasil dibaca: %d baris\n", rows);
    
    // Cari tahun yang hilang
    int missing_years[MISSING_YEARS] = {2005, 2006, 2015, 2016};
    
    // Urutkan data berdasarkan tahun
    qsort(data, rows, sizeof(DataRow), compareDataRows);
    
    // Menyiapkan array untuk regresi persentase pengguna internet
    int original_years_internet[MAX_ROWS];
    double normalized_years_internet[MAX_ROWS];
    double percentages[MAX_ROWS];
    int n_internet = 0;
    int base_year = 2000; // Tahun dasar untuk normalisasi
    
    // Menyiapkan array untuk regresi populasi
    int original_years_population[MAX_ROWS];
    double normalized_years_population[MAX_ROWS];
    double populations[MAX_ROWS];
    int n_population = 0;
    
    // Filter data setelah tahun 2000 untuk persentase internet
    // Ini mengikuti pola pemfilteran pada kode original
    for (int i = 0; i < rows; i++) {
        if (data[i].year >= 2000 && data[i].percentage > 0) {
            original_years_internet[n_internet] = data[i].year;
            percentages[n_internet] = data[i].percentage;
            n_internet++;
        }
        
        // Gunakan semua data untuk populasi
        original_years_population[n_population] = data[i].year;
        populations[n_population] = data[i].population;
        n_population++;
    }
    
    // Normalisasi tahun untuk membuat perhitungan lebih stabil
    normalizeYears(original_years_internet, normalized_years_internet, n_internet, base_year);
    normalizeYears(original_years_population, normalized_years_population, n_population, base_year);
    
    // Derajat polinomial
    int degree_internet = 3; // Derajat polinomial untuk persentase internet
    int degree_population = 2; // Derajat polinomial untuk populasi
    
    // Array untuk koefisien polinomial
    double* coef_internet = (double*)malloc((degree_internet + 1) * sizeof(double));
    double* coef_population = (double*)malloc((degree_population + 1) * sizeof(double));
    
    // Melakukan regresi polinomial untuk persentase pengguna internet
    polynomialRegression(normalized_years_internet, percentages, n_internet, coef_internet, degree_internet);
    
    printf("\nHasil Regresi Polinomial (Persentase Internet):\n");
    printf("Model: y = ");
    for (int i = 0; i <= degree_internet; i++) {
        if (i == 0) {
            printf("%.6f", coef_internet[i]);
        } else {
            printf(" %c %.6f * (x - %d)^%d", (coef_internet[i] >= 0) ? '+' : '-', 
                fabs(coef_internet[i]), base_year, i);
        }
    }
    printf("\n");
    
    // Melakukan regresi polinomial untuk populasi
    polynomialRegression(normalized_years_population, populations, n_population, coef_population, degree_population);
    
    printf("\nHasil Regresi Polinomial (Populasi):\n");
    printf("Model: y = ");
    for (int i = 0; i <= degree_population; i++) {
        if (i == 0) {
            printf("%.2f", coef_population[i]);
        } else {
            printf(" %c %.2f * (x - %d)^%d", (coef_population[i] >= 0) ? '+' : '-', 
                fabs(coef_population[i]), base_year, i);
        }
    }
    printf("\n\n");
    
    // Memprediksi nilai untuk tahun yang hilang
    printf("Prediksi untuk Tahun yang Hilang:\n");
    printf("%-6s %-25s %-15s\n", "Tahun", "Persentase Pengguna Internet", "Populasi");
    
    // Tambahkan data yang hilang ke dataset
    DataRow new_data[MAX_ROWS + MISSING_YEARS];
    int new_rows = 0;
    
    // Salin data yang ada ke dataset baru
    for (int i = 0; i < rows; i++) {
        new_data[new_rows++] = data[i];
    }
    
    // Tambahkan prediksi untuk tahun yang hilang
    for (int i = 0; i < MISSING_YEARS; i++) {
        int year = missing_years[i];
        double normalized_year = (double)(year - base_year);
        
        // Prediksi persentase internet menggunakan model polinomial
        double predicted_percentage = evaluatePolynomial(normalized_year, coef_internet, degree_internet);
        if (predicted_percentage < 0) predicted_percentage = 0; // Koreksi nilai negatif
        if (predicted_percentage > 100) predicted_percentage = 100; // Koreksi nilai di atas 100%
        
        // Prediksi populasi menggunakan model polinomial
        double predicted_population = evaluatePolynomial(normalized_year, coef_population, degree_population);
        if (predicted_population < 0) predicted_population = 0; // Koreksi nilai negatif
        
        // Tambahkan ke dataset baru
        new_data[new_rows].year = year;
        new_data[new_rows].percentage = predicted_percentage;
        new_data[new_rows].population = predicted_population;
        new_rows++;
        
        printf("%-6d %-25.6f %-15.0f\n", year, predicted_percentage, predicted_population);
    }
    
    // Urutkan dataset baru berdasarkan tahun
    qsort(new_data, new_rows, sizeof(DataRow), compareDataRows);
    
    // Simpan dataset lengkap ke file baru
    writeCSV("Data_Lengkap_Hasil_Polinomial.csv", new_data, new_rows);
    
    printf("\nData lengkap telah disimpan ke file 'Data_Lengkap_Hasil_Polinomial.csv'\n");
    
    // Bersihkan memori
    free(coef_internet);
    free(coef_population);
    
    return 0;
}