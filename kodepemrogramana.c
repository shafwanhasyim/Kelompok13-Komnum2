/**
 * Program untuk melengkapi data yang hilang pada tahun 2005, 2006, 2015, dan 2016
 * menggunakan metode pencocokan kurva eksponensial.
 * 
 * Metode ini menggunakan:
 * - Regresi eksponensial (y = a * e^(b*x)) untuk data persentase pengguna internet
 * - Regresi linear untuk data populasi
 * 
 * Penggunaan model eksponensial lebih sesuai untuk data persentase internet
 * karena pertumbuhan pengguna internet cenderung mengikuti pola eksponensial.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define MAX_ROWS 100
#define MAX_YEARS 100
#define MISSING_YEARS 4

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

// Fungsi untuk melakukan regresi linear
void linearRegression(double x[], double y[], int n, double* a, double* b) {
    double sum_x = 0, sum_y = 0, sum_xy = 0, sum_x2 = 0;
    
    for (int i = 0; i < n; i++) {
        sum_x += x[i];
        sum_y += y[i];
        sum_xy += x[i] * y[i];
        sum_x2 += x[i] * x[i];
    }
    
    *b = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x);
    *a = (sum_y - *b * sum_x) / n;
}

// Fungsi untuk melakukan regresi eksponensial (y = a * e^(b*x))
void exponentialRegression(double x[], double y[], int n, double* a, double* b) {
    double* ln_y = (double*)malloc(n * sizeof(double));
    
    // Transformasi ln(y) untuk data y > 0
    int valid_points = 0;
    for (int i = 0; i < n; i++) {
        if (y[i] > 0) {
            ln_y[valid_points] = log(y[i]);
            x[valid_points] = x[i];
            valid_points++;
        }
    }
    
    // Lakukan regresi linear pada ln(y) = ln(a) + b*x
    double ln_a, b_value;
    linearRegression(x, ln_y, valid_points, &ln_a, &b_value);
    
    *a = exp(ln_a);
    *b = b_value;
    
    free(ln_y);
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
    
    // Melakukan regresi eksponensial untuk persentase pengguna internet
    double a_internet, b_internet;
    exponentialRegression(normalized_years_internet, percentages, n_internet, &a_internet, &b_internet);
    
    printf("\nHasil Regresi Eksponensial (Persentase Internet):\n");
    printf("Model: y = %.6f * e^(%.6f * (x - %d))\n", a_internet, b_internet, base_year);
    
    // Melakukan regresi linear untuk populasi
    double a_population, b_population;
    linearRegression(normalized_years_population, populations, n_population, &a_population, &b_population);
    
    printf("\nHasil Regresi Linear (Populasi):\n");
    printf("Model: y = %.2f + %.2f * (x - %d)\n\n", a_population, b_population, base_year);
    
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
        
        // Prediksi persentase internet menggunakan model eksponensial
        double predicted_percentage = a_internet * exp(b_internet * normalized_year);
        
        // Prediksi populasi menggunakan model linear
        double predicted_population = a_population + b_population * normalized_year;
        
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
    writeCSV("Data_Lengkap_Hasil_Eksponensial.csv", new_data, new_rows);
    
    printf("\nData lengkap telah disimpan ke file 'Data_Lengkap_Hasil_Eksponensial.csv'\n");
    
    return 0;
}