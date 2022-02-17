# Dashboard-Marketing
Code untuk membuat sistem informasi manajemen menggunakan R Shiny.
Data yang digunakan adalah data dummy dengan struktur hampir sama dengan laporan keuangan.
Data disimpan dalam google sheet yang kemudian dipanggil dalam aplikasi.
Aplikasi terdiri atas :
a.	Form Login digunakan untuk melindungi agar data tidak dapat diakses oleh orang yang tidak berkepentingan. Untuk masuk ke aplikasi (website), pengguna diharuskan mengisi form login terlebih dahulu sehingga dapat mengakses keseluruhan menu di dalam aplikasi (website).
b.	Navigation Pane digunakan untuk mengakses beberapa halaman informasi yang terdiri dari Home Page, Dashboard, RKAP, Form, Summary dan Data.
c.	Home Page (Navigation Pane Pertama) 
Ketika Menu Home Page diklik maka akan muncul tampilan yang berisi informasi umum tentang PT Pelindo Daya Sejahtera.
d.	Dashboard 
Menu selanjutnya adalah Dashboard yang berisi grafik kinerja divisi komersial yang dibagi dalam beberapa panel berdasarkan bulan dan tahun, berdasarkan PIC, berdasarkan Regional, berdasarkan jenis perjanjian, dan berdasarkan kategori pekerjaan. Perbedaan antar tab “All”, “PIC” hingga “Kategori Perjanjian” terletak pada checkbox yang digunakan sebagai filter data dalam menampilkan grafik. 
e.	RKAP. Pada menu RKAP berisi realisasi pendapatan usaha yang dapat dilihat per bulan dan per tahun.
f.	Form yang berguna untuk input data Pekerjaan
g.	Summary (Ringkasan) berisi total pendapatan usaha dan manajemen fee dalam satu tahun untuk setiap pekerjaan.
h.	Data (Data Lengkap) berisi semua data pekerjaan setiap bulannya.
