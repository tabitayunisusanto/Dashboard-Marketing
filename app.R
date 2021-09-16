library(shiny)
library(shinydashboard)
library(purrr)
library(shinyWidgets)
library(shinymanager)
library(plotrix)
library(ggpubr)
library(shinyMatrix)
library(leaflet)
library(googleway)
library(googlesheets4)
library(data.table)
library(png)
library(shinydashboardPlus)
library(DT)
library(gt)
library(shinyjs)
library(lubridate)
library(scales)
library(dplyr)
library(plotly)
library(summarytools)
library(ggplot2)
library(reshape2)
library(rhandsontable)
library(ggforce)
library(magick)
library(png)
library(scales)
library(gridExtra)
library(gganimate)


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
    user = c("user"), password = c("password"),
    stringsAsFactors = FALSE
)


labelMandatory <- function(label) {
    tagList(label,
        span("*", class = "mandatory_star"))}

appCSS <-
    ".mandatory_star { color: red; }"

fieldsMandatory <- c("Kat","os_type","pic_type","ket_prog","reg_type","kota","Nmprs","Nmpkrj","prog_pkj","st_kon")

fields=c("date1","date2","Nmpkrj","Nmprs","Kat","pic_type","os_type")

options(gargle_oauth_cache = ".secrets", email = "tabitayuni100@gmail.com")

gs4_auth(cache = ".secrets", email = "tabitayuni100@gmail.com")

as_sheets_id("https://docs.google.com/spreadsheets/d/1nnm1ydjtt0YwyxuQXX8se-L3z9aMjBDdosdsF4GfEgM/edit#gid=0")
sheet_id <- "1nnm1ydjtt0YwyxuQXX8se-L3z9aMjBDdosdsF4GfEgM"

resetForm <- function(session) {
  # reset values
  updateDateInput(session,"date1", value =NULL)
  updateDateInput(session, "date2", value = NULL)
  updateSelectInput(session,"os_type")
  updateSelectInput(session,"pic_type")
  updateSelectInput(session,"reg_type")
  updateTextInput(session,"Nmpkrj",value = "")
  updateTextInput(session,"Nmprs",value = "")
  updateNumericInput(session,"Kat",value=NULL)
  updateNumericInput(session,"man_fee",value=NULL)
  updateAirDateInput(session,"tgl_a", value =NULL)
  updateAirDateInput(session,"tgl_b", value =NULL)
}
library(formattable)

jscode <- HTML("
$(document).on('shiny:connected', function(event) {
  $('.sidebar-toggle').on('click', function() {
    if ($('body')[0].className != 'skin-blue sidebar-mini sidebar-collapse') {
      $('#sidebarCollapsed').css('display', 'none')
      $('nav.navbar-static-top').css('width', '1800px')
      $('nav.navbar-static-top').css('margin-left', '0px')
      $('header.main-header').css('width', '1800px')
      $('.sidebar-toggle').css('position', 'relative')
      $('span.logo').css('display', 'none')
    } else {
      $('#sidebarCollapsed').css('display', 'block')
      $('nav.navbar-static-top').css('margin-left', '230px')
      $('header.main-header').css('width', '884.44px')
      $('nav.navbar-static-top').css('width', '1800.44px')
      $('span.logo').css('display', 'block')
    }
  })
});
")

csscode <- HTML("
.sidebar-mini.sidebar-collapse .content-wrapper {
      margin-left: 0px !important;
}
")

css1 <- HTML(
  "/* move logo to center */
    #logo {
        position: absolute;
        left: 65%;
        top: 50%;
        transform: translate(-50%, -50%);
    }
    /* remove hover effect */
    #logo > a:hover {
        background-color: white !important;
        color: transparant !important;
    }"
)

dz1 <- data.frame(read_sheet(ss = sheet_id,sheet = "DATA"))

rs=read_sheet(ss = sheet_id,sheet = "DATA")
shinyApp( ui =secure_app(head_auth = tags$script(inactivity),
                   dashboardPage(
                     dashboardHeader(title="Menu",tags$li(class = "dropdown",
                                             id = "logo",
                                             tags$a(href = 'https://www.pelindo.co.id/id',
                                                    img(src = 'https://seeklogo.com/images/P/pt-pelabuhan-indonesia-iii-persero-logo-FC4E3B9D8C-seeklogo.com.png',
                                                        title = "Company Home", height = "30px"),
                                                    style = "padding-top:5px; padding-bottom:5px;"),
                                             tags$style(css1)
                     )),

  dashboardSidebar(sidebarMenu(id='sidebar',
      menuItem("Home Page",tabName="HP", icon = icon("home")),                               
       menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar"),badgeLabel = "Dashboard", badgeColor = "teal"),
       menuItem("RKAP", icon = icon("money",lib = "font-awesome"), tabName = "rkap",badgeLabel = "RKAP", badgeColor = "orange"),
       menuItem("Form", icon = icon("clipboard-list"), tabName = "FORM",badgeLabel = "Form", badgeColor = "green"),
       menuItem("Summary", tabName = "Ringkasan", icon = icon("audible", lib = "font-awesome"),badgeLabel = "Data (Ringkasan)", badgeColor = "blue"),
       menuItem("Data", tabName = "Data", icon = icon("book", lib = "font-awesome"),badgeLabel = "Data (Lengkap)", badgeColor = "maroon")),
       uiOutput('style_tag'),
       collapsed = TRUE,
       tags$head(tags$script(jscode)),
       tags$head(tags$style(csscode))),

  dashboardBody(
     tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 28px;
        line-height: 50px;
        text-align: center;
        font-family: Palatino;
        padding: 0 15px;
        overflow: unhidden;
        color: white;
    }
    .logo {
      background-color: #A9A9A9 !important;
                              }
      .navbar {
      background-color: #A9A9A9 !important;
                              }
    '))),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> KOMERSIAL DAN PEMASARAN </span>\')
      })
     ')),

  tabItems
(tabItem (tabName = "FORM",
         titlePanel(h1(id="big-heading", strong("FORM PEKERJAAN KOMERSIAL DAN PEMASARAN"),align = "center")),
         tags$style(HTML("#big-heading{color: BLUE;}")),

         shinyjs::useShinyjs(),
         shinyjs::inlineCSS(appCSS),
         
         div(id = "form",
         fluidRow(
column(2,            
         dateInput("date1", "Bulan", value =NULL,format = "MM"),
        dateInput("date2", "Tahun", value =NULL,format = "yyyy"),
        numericInput("Kat", labelMandatory("Jumlah Kategori Pekerjaan"),value = 1, min = 1,max=8),
        selectInput("os_type",labelMandatory("Jenis Perjanjian"), c("",  "Alih Daya", "Pemborongan", "Project PDC","Canvasing")),
        uiOutput("conditional_comment1"),
        uiOutput("conditional_comment2"),
        uiOutput("conditional_comment3"),
        selectInput("pic_type",labelMandatory("Penanggung Jawab"), c("",  "D", "A", "B","C","All","Not Available")),
        selectInput("reg_type", labelMandatory("Regional"),  c("",  "Jawa Timur", "Jawa Tengah", "Kalimantan","Banyuwangi Bali Nusra Tenggara", "Non Captive")),
        textInput("kota", labelMandatory("Kota"), ""),
       textInput("Nmprs", labelMandatory("Nama Perusahaan"), ""),
       textInput("Nmpkrj", labelMandatory("Nama Pekerjaan"), ""),
       airDatepickerInput("tgl_a", labelMandatory("Tanggal Mulai Pekerjaan"),
                          view = "day", #editing what the popup calendar shows when it opens
                          minView = "day", #making it not possible to go down to a "days" view and pick the wrong date
                          dateFormat = "dd/mm/yyyy"),
       airDatepickerInput("tgl_b", labelMandatory("Tanggal Kontrak Terbit"), view = "day",  minView = "day", dateFormat = "dd/mm/yyyy"),
       uiOutput("tgl_c"),
       numericInput("prog_pkj", labelMandatory("Persentase Progress Pekerjaan (%)"),value = 100, min = 0 ),
       selectInput("st_kon", labelMandatory("Status Kontrak"),  c("",  "Aktif", "Non Aktif", "Putus")),
       textInput("ket_prog", labelMandatory("Keterangan Progress"), "")
        ),
        
column(2,selectInput("cek_type1", labelMandatory("Kategori Pekerjaan 1"),
                     choices=c("","Op1", "Op2", "AP", "CS", "Pg","PP",
                               "Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")),
       uiOutput("condPanels11"),
       uiOutput("condPanels12"),
       uiOutput("condPanels13"),
       uiOutput("condPanels14"),
       uiOutput("condPanels15"),
       uiOutput("condPanels16"),
       uiOutput("condPanels17")
         ),
                                                               
column(2, 
       uiOutput("condPanels51"),
       uiOutput("condPanels52"),
       uiOutput("condPanels53"),
       uiOutput("condPanels54"),
       uiOutput("condPanels55"),
       uiOutput("condPanels56"),
       uiOutput("condPanels57"),
       uiOutput("condPanels58"),
       
       uiOutput("condPanels61"),
       uiOutput("condPanels62"),
       uiOutput("condPanels63"),
       uiOutput("condPanels64"),
       uiOutput("condPanels65"),
       uiOutput("condPanels66"),
       uiOutput("condPanels67"),
       uiOutput("condPanels68")
       ),

column(2, numericInput("biaya_peng1", labelMandatory("Biaya Pengolahan 1"),value=NULL),
       uiOutput("condPanels21"),
       uiOutput("condPanels22"),
       uiOutput("condPanels23"),
       uiOutput("condPanels24"),
       uiOutput("condPanels25"),
       uiOutput("condPanels26"),
       uiOutput("condPanels27"),
       
       uiOutput("condPanels71"),
       uiOutput("condPanels72"),
       uiOutput("condPanels73"),
       uiOutput("condPanels74"),
       uiOutput("condPanels75"),
       uiOutput("condPanels76"),
       uiOutput("condPanels77"),
       uiOutput("condPanels78")
       ),

column(2,  numericInput("man_fee1", labelMandatory("Manajemen Fee 1"),value=NULL),
       uiOutput("condPanels31"),
       uiOutput("condPanels32"),
       uiOutput("condPanels33"),
       uiOutput("condPanels34"),
       uiOutput("condPanels35"),
       uiOutput("condPanels36"),
       uiOutput("condPanels37"),
       
       uiOutput("condPanels81"),
       uiOutput("condPanels82"),
       uiOutput("condPanels83"),
       uiOutput("condPanels84"),
       uiOutput("condPanels85"),
       uiOutput("condPanels86"),
       uiOutput("condPanels87"),
       uiOutput("condPanels88")
       ),

column(2,  numericInput("tenaga_kerja1", labelMandatory("Tenaga Kerja/Paket 1"),value=NULL),
       uiOutput("condPanels41"),
       uiOutput("condPanels42"),
       uiOutput("condPanels43"),
       uiOutput("condPanels44"),
       uiOutput("condPanels45"),
       uiOutput("condPanels46"),
       uiOutput("condPanels47"),
       
       uiOutput("condPanels91"),
       uiOutput("condPanels92"),
       uiOutput("condPanels93"),
       uiOutput("condPanels94"),
       uiOutput("condPanels95"),
       uiOutput("condPanels96"),
       uiOutput("condPanels97"),
       uiOutput("condPanels98"),
       
       actionButton("submit", "Submit"),
       actionButton("clear", "Clear Form"))
      
)
)),

tabItem(tabName = "rkap",
        fluidRow( column(2 ,selectInput("bln1", "Bulan", choices=c("","January","February", "March",
          "April", "May", "June", "July", "August", "September", "October", "November", "December"))),        
        column(2, numericInput("thn1",  value = 2021, min = 2000, label = "Tahun")),
        column(1, actionButton("cek", "CEK!",icon = icon("check-double")))),
        h3(id="big5", strong("REALISASI PENDAPATAN USAHA"),align = "center"),
        downloadButton("downloadData", "Download"),
       tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
       div(dataTableOutput("rkp"), style = "font-size:95%")),   

tabItem(tabName = "HP", 
          h2(strong("PT PELINDO DAYA SEJAHTERA"),align = "center"),
          h2(),
          p("PT Pelindo Daya Sejahtera yang selanjutnya disebut PT PDS adalah sebuah Perusahaan yang berorientasi bisnis yang tidak terlepas dari Risiko Usaha dalam mengembangkan usahanya. Sebagai Anak Perusahaan Holding Pelindo, PT PDS wajib mengelola kegiatan usahanya secara efektif, efisien dan produktif serta mempertimbangkan Risiko usaha.
            Pendirian PT Pelindo Daya Sejahtera (PDS) merupakan sebuah bentuk komitmen PT Pelabuhan Indonesia III untuk meningkatkan kesejahteraan tenaga alih daya. Kesejahteraan tenaga kerja alih daya dapat diwujudkan melalui pengelolaan sumber daya manusia secara tepat dan professional, tanpa mengabaikan azas kepatuhan terhadap peraturan perundang-undangan yang berlaku. Pendirian PDS dilakukan melalui akuisisi PT Persada Jasa Utama (PJU), perusahaan milik Koperasi Pelabuhan Indonesia III oleh PT Pelabuhan Indonesia III (Persero) di depan Notaris Yatiningsih, SH., MH. dan tertuang dalam Akta Pergantian Nama Perseroan No.183 pada tanggal 19 Maret 2014. Hari bersejarah tersebut kemudian ditetapkan sebagai hari jadi PDS.", style = "font-family: 'times'; font-si10pt", align="justify"),
          br(),
          p("Komitmen", style = "font-family: 'times'; font-si10pt",align="justify"),
          p("PDS berkomitmen menjaga kualitas serta kompetensi tenaga kerja demi memberikan pelayanan terbaik bagi pengguna jasa. Komitmen tersebut kami wujudkan melalui kepatuhan terhadap peraturan perundang-undangan yang berlaku dan melindungi hak serta kewajiban karyawan serta pengguna jasa.", style = "font-family: 'times'; font-si10pt",align="justify"),
          br(),
          p("Visi :", style = "font-family: 'times'; font-si10pt",align="justify"),
          p("Menjadi partner terpilih dalam menunjang bisnis, memberikan pelayanan dan solusi.", style = "font-family: 'times'; font-si10pt"),
          br(),
          p("Misi :", style = "font-family: 'times'; font-si10pt"),
          p("1. Memberikan pelayanan dan solusi bagi mitra bisnis dan pemegang saham berdasarkan pelayanan prima, teknologi dan sumber daya manusia yang berkualitas.", style = "font-family: 'times'; font-si10pt"),
          p("2. Senantiasa berinovasi, bertransformasi terhadap perubahan yang memberikan perkembangan dan kesinambungan melalui diversifikasi bisnis.", style = "font-family: 'times'; font-si10pt"),
          br(),
          p("Legalitas dan perizinan :", style = "font-family: 'times'; font-si10pt"),
          p("Akta pendirian Perseroan Terbatas PT Persada Jasa Utama Nomor 41 tanggal 10 Maret 2008 yang dibuat dihadapan Notaris Inas Abdullah Thalib, S.H sebagaimana telah disahkan dengan Keputusan Menteri Hukum dan Hak Asasi Manusia RI Nomor AHU-18839 AH.01.01 tahun 2008 tanggal 16 April 2008, dan perubahan akta yang terakhir melaui Akta Pernyataan Keputusan Sirkuler Para Pemegang Saham di Luar Rapat Umum Pemegang Saham Luar Biasa PT Pelindo Daya Sejahtera Nomor 03 Tanggal 4 Desember 2019 yang dibuat di hadapan Notaris Didit Aditya Hermawanto SH., M.Kn sebagaimana telah disahkan melalui Surat Kementerian Hukum dan HAM Perihal Penerimaan Pemberitahuan Perubahan Data Perseroan PT Pelindo Daya Sejahtera Nomor : AHU-AH.01.03-0001775 tanggal 3 Januari 2020.", style = "font-family: 'times'; font-si10pt"),
          br(),
          p("Kinerja PDS telah diapresiasi dalam beberapa penghargaan dan sertifikasi, antara lain :", style = "font-family: 'times'; font-si10pt"),
          p("1. Indonesia Entrepreneur and Education Award 2015 dari Kementerian Koperasi dan Usaha Kecil Menengah", style = "font-family: 'times'; font-si10pt"),
          p("2. Indonesia Business Quality Award 2015 dari Indonesia Development Achievement Foundation", style = "font-family: 'times'; font-si10pt"),
          p("3. The Most Trusted Company in Services Excellent of The Year 2016 Business Challenges Award", style = "font-family: 'times'; font-si10pt"),
          p("4. The Best Improvement Outsorcing Company of The Year 2016 Anugerah Citra Indonesia", style = "font-family: 'times'; font-si10pt"),
          p("5. The Best Inspiring Leader In Satisfactory Performance Of The Year 2017", style = "font-family: 'times'; font-si10pt"),
          p("6. Zero Accident Award dari Disnakertrans Jawa Timur", style = "font-family: 'times'; font-si10pt"),
          p("7. Sertifikasi ISO 9001:2015 tentang Sistem Manajemen Mutu", style = "font-family: 'times'; font-si10pt"),
          br(),
          p("Layanan :", style = "font-family: 'times'; font-si10pt"),
          p("1. Layanan Jasa Tenaga Kerja", style = "font-family: 'times'; font-si10pt"),
          p("2. Layanan Asesmen, Pelatihan dan Konsultasi", style = "font-family: 'times'; font-si10pt"),
          p("3. Layanan Pemeliharaan Fasilitas", style = "font-family: 'times'; font-si10pt"),
          br(),
          p("Manajemen Mutu", style = "font-family: 'times'; font-si10pt"),
          p("PDS siap memberikan pelayanan terbaik kepada pengguna jasa. Pelayanan prima juga didukung dengan mekanisme penanganan keluhan pelanggan, upaya preventif dan korektif, serta peningkatan mutu secara berkesinambungan. Sebagai jaminan atas komitmen kami terhadap kepuasan pelanggan, Standar Manajemen Mutu PDS telah mendapatkan sertifikat ISO 9001:2015 untuk Sistem Manajemen mutu.", style = "font-family: 'times'; font-si10pt")
  ),
                                                   
tabItem(tabName = "Data",
       actionButton("refresh", "REFRESH TABEL", icon = icon("refresh")),
       uiOutput("thankyoutext"),
        dataTableOutput("res"), tags$hr(),
       tags$script(HTML('$(document).on("click", "input", function () {
  var checkboxes = document.getElementsByName("row_selected");
  var checkboxesChecked = [];
  for (var i=0; i<checkboxes.length; i++) {
     if (checkboxes[i].checked) {
        checkboxesChecked.push(checkboxes[i].value);
     }
  }
  Shiny.onInputChange("checked_rows",checkboxesChecked);
      })')),
       tags$script("$(document).on('click', '#res button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())
  });")
       
       
       ),
  
tabItem(tabName = "Ringkasan",
        fluidRow(  actionButton("refresh1", "REFRESH TABEL", icon = icon("refresh"))),
        h2(id="big1", strong("DAFTAR PEKERJAAN"),align = "center"),
          fluidRow( div(dataTableOutput("ris"), style = "font-size:95%"), tags$hr())),
  
tabItem(tabName = "dashboard",
     fluidRow(column(2,htmlOutput("picture")),
     column(1,actionButton("refresh2", "REFRESH", icon = icon("refresh"))),
     h1(id="big3", strong("KINERJA DIVISI KOMERSIAL"),align = "center"),
     tags$style(HTML("#big3{color: #FF0000;}"))),
     tabsetPanel(
       tabPanel("ALL",
                fluidRow(
     box(background = "black", title ="Pilih Di Sini",width = 2, icon = icon("users"), collapsible = FALSE,
         
         uiOutput("checktahun"),
        uiOutput("checkbulan")
        ),
     valueBoxOutput("progressBox", width=2 ),
     valueBoxOutput("tot_pen", width=2 ),
     valueBoxOutput("tot_bi", width=2 ),
     valueBoxOutput("tot_man", width=2 ),
     valueBoxOutput("stat_non", width=2 ),
     
     box(title = strong("REALISASI VS TARGET PENDAPATAN USAHA "),plotOutput("graf1performa4"),width =5),
     box(title = strong("Pendapatan Usaha Berdasarkan PIC") , imageOutput("graf11"),width =5)),
     
     fluidRow(
     box(title = strong("TOTAL PENDAPATAN USAHA BY REGIONAL"),plotOutput("graf1performa"),width = 4),
     box(title = strong("BIAYA PENGELOLAAN BY REGIONAL"),plotOutput("graf1performa1"),width = 4),
     box(title = strong("MANAJEMEN FEE BY REGIONAL"),plotOutput("graf1performa2"),width = 4),
     box(title = strong("PENDAPATAN USAHA PIC BY REGIONAL"),plotOutput("graf12"),width =5),
     box(title = strong("PERSENTASE PENDAPATAN USAHA"),plotOutput("graf1performa3"),width =4),
     valueBoxOutput("stat_akt", width=2 )
         )
        ),
     
     tabPanel("PIC",
     fluidRow(
     box(background = "black", title ="Pilih",width = 2, icon = icon("users"), collapsible = FALSE,
     uiOutput("checkpic")),
     box(title = strong("PENDAPATAN USAHA JENIS PERJANJIAN BY REGIONAL"),plotOutput("graf31"),width =6)
              ) ),
     
     tabPanel("REGIONAL",
              fluidRow(
                box(background = "black", title ="Pilih Di Sini",width = 2, icon = icon("users"), collapsible = FALSE,
                    uiOutput("checkreg")),
                box(title = strong("BIAYA PENGELOLAAN VS MANAJEMEN FEE BY PEKERJAAN"),plotOutput("graf21"),width = 5)
              ) ),
     
     tabPanel("JENIS PERJANJIAN",
              fluidRow(
                box(background = "black", title ="Pilih Di Sini",width = 2, icon = icon("users"), collapsible = FALSE,
                    uiOutput("checkperj")),
                valueBoxOutput("jum_pem", width=2 ),
                valueBoxOutput("jum_ad", width=2 ),
                valueBoxOutput("jum_pdc", width=2 ),
                valueBoxOutput("jum_can", width=2 ),
                box(title = strong("PENDAPATAN USAHA PIC BY REGIONAL"),plotOutput("graf41"),width =6)
              )),
     
     tabPanel("KATEGORI PEKERJAAN",
              fluidRow(
                box(background = "black", title ="Pilih Di Sini",width = 2, icon = icon("users"), collapsible = FALSE,
                    uiOutput("checkkat")),
                
                box(title = strong("BIAYA PENGELOLAAN VS MANAJEMEN FEE BY PIC"),plotOutput("graf51"),width = 5)
              ))
              )))))),

server = function(input, output, session) {
        result_auth <- secure_server(check_credentials = check_credentials(credentials))
        observe({
            mandatoryFilled <-vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
      
        vals<-reactiveValues()
        vals$Data<-as.data.table(read_sheet(ss = sheet_id,sheet = "DATA"))
        
        output$style_tag <- renderUI({
          if(input$sidebar=='dashboard')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:#FFFAFA;}'))))
          if(input$sidebar=='FORM')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:rgb(240, 248, 255);}'))))
          if(input$sidebar=='Data')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:#ffffff;}'))))
          if(input$sidebar=='rkap')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:#e6fff2;}'))))
          if(input$sidebar=='Ringkasan')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:#ffffe6;}'))))
          if(input$sidebar=='HP')
            return(tags$head(tags$style(HTML('.content-wrapper {background-color:#e1eaea;}'))))
          })
        
        output$tgl_c=renderUI({
        airDatepickerInput("tgl_c", labelMandatory("Tanggal Kontrak Berakhir"), view = "day", minView = "day",
              dateFormat = "dd/mm/yyyy",minDate = input$tgl_b+1)  
        })
        
        output$conditional_comment1 <- renderUI({
            req(input$os_type== "Alih Daya"|input$os_type== "Pemborongan")
        textAreaInput(inputId = "no_prs", labelMandatory("No Kontrak Perusahaan"), 
                      placeholder = "Nomor Kontrak Perusahaan")})
        
        output$conditional_comment2<- renderUI({
          req(input$os_type== "Alih Daya"|input$os_type== "Pemborongan")
          textAreaInput(inputId = "no_pds", labelMandatory("No Kontrak PDS"), placeholder = "Nomor Kontrak PDS")})
        
      output$conditional_comment3<- renderUI({
       req(input$os_type== "Project PDC")
       textAreaInput(inputId = "spnwr", labelMandatory("Surat Penawaran"), placeholder = "Nomor Surat Penawaran")})
        
      output$condPanels11 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ",
      selectInput("cek_type2", labelMandatory("Kategori Pekerjaan 2"),choices=c("","Op1", "Op2", "AP", "CS", "Pg",
      "PP","Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")))})
      output$condPanels12 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ",
      selectInput("cek_type3", labelMandatory("Kategori Pekerjaan 3"),choices=c("","Op1", "Op2", "AP", "CS", "Pg",
      "PP","Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")))})
      output$condPanels13 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ",
      selectInput("cek_type4", labelMandatory("Kategori Pekerjaan 4"),choices=c("","Op1", "Op2", "AP", "CS", "Pg",
      "PP","Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")))})
      output$condPanels14 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ",
      selectInput("cek_type5", labelMandatory("Kategori Pekerjaan 5"),choices=c("","Op1", "Op2", "AP", "CS", "Pg",
      "PP","Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")))})
      output$condPanels15 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 6 ",
      selectInput("cek_type6", labelMandatory("Kategori Pekerjaan 6"),choices=c("","Op1", "Op2", "AP", "CS", "Pg",
      "PP","Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")))})
      output$condPanels16 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 7 ",
      selectInput("cek_type7", labelMandatory("Kategori Pekerjaan 7"),choices=c("","Op1", "Op2", "AP", "CS", "Pg",
      "PP","Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")))})
      output$condPanels17 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ",
      selectInput("cek_type8", labelMandatory("Kategori Pekerjaan 8"),choices=c("","Op1", "Op2", "AP", "CS", "Pg",
      "PP","Sewa Kendaraan","Luggage Handling","Pendidikan dan Pelatihan","Pendapatan Rekruitment","Pendapatan Assesment","Pendapatan Consultant")))})
      
      output$condPanels21 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("biaya_peng2", labelMandatory("Biaya Pengolahan 2"),value=NULL))})
      output$condPanels22 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("biaya_peng3", labelMandatory("Biaya Pengolahan 3"),value=NULL))})
      output$condPanels23 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("biaya_peng4", labelMandatory("Biaya Pengolahan 4"),value=NULL))})
      output$condPanels24 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("biaya_peng5", labelMandatory("Biaya Pengolahan 5"),value=NULL))})
      output$condPanels25 <- renderUI({
        conditionalPanel(condition = "input.Kat >=6 ", numericInput("biaya_peng6", labelMandatory("Biaya Pengolahan 6"),value=NULL))})
      output$condPanels26 <- renderUI({
        conditionalPanel(condition = "input.Kat >=7 ", numericInput("biaya_peng7", labelMandatory("Biaya Pengolahan 7"),value=NULL))})
      output$condPanels27 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("biaya_peng8", labelMandatory("Biaya Pengolahan 8"),value=NULL))})
      
      output$condPanels31 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("man_fee2", labelMandatory("Manajemen Fee 2"),value=NULL))})
      output$condPanels32 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("man_fee3", labelMandatory("Manajemen Fee 3"),value=NULL))})
      output$condPanels33 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("man_fee4", labelMandatory("Manajemen Fee 4"),value=NULL))})
      output$condPanels34 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("man_fee5", labelMandatory("Manajemen Fee 5"),value=NULL))})
      output$condPanels35 <- renderUI({
        conditionalPanel(condition = "input.Kat >=6 ", numericInput("man_fee6", labelMandatory("Manajemen Fee 6"),value=NULL))})
      output$condPanels36 <- renderUI({
        conditionalPanel(condition = "input.Kat >=7 ", numericInput("man_fee7", labelMandatory("Manajemen Fee 7"),value=NULL))})
      output$condPanels37 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("man_fee8", labelMandatory("Manajemen Fee 8"),value=NULL))})
      
      output$condPanels41 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("tenaga_kerja2", labelMandatory("Tenaga Kerja/Paket 2"),value=NULL))})
      output$condPanels42 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("tenaga_kerja3", labelMandatory("Tenaga Kerja/Paket 3"),value=NULL))})
      output$condPanels43 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("tenaga_kerja4", labelMandatory("Tenaga Kerja/Paket 4"),value=NULL))})
      output$condPanels44 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("tenaga_kerja5", labelMandatory("Tenaga Kerja/Paket 5"),value=NULL))})
      output$condPanels45 <- renderUI({
        conditionalPanel(condition = "input.Kat >=6 ", numericInput("tenaga_kerja6", labelMandatory("Tenaga Kerja/Paket 6"),value=NULL))})
      output$condPanels46 <- renderUI({
        conditionalPanel(condition = "input.Kat >=7 ", numericInput("tenaga_kerja7", labelMandatory("Tenaga Kerja/Paket 7"),value=NULL))})
      output$condPanels47 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("tenaga_kerja8", labelMandatory("Tenaga Kerja/Paket 8"),value=NULL))})
      
      
      output$condPanels51 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 1 ", numericInput("upah1", labelMandatory("TOTAL UPAH 1"),value=NULL))})
      output$condPanels52 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("upah2", labelMandatory("TOTAL UPAH 2"),value=NULL))})
      output$condPanels53 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("upah3", labelMandatory("TOTAL UPAH 3"),value=NULL))})
      output$condPanels54 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("upah4", labelMandatory("TOTAL UPAH 4"),value=NULL))})
      output$condPanels55 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("upah5", labelMandatory("TOTAL UPAH 5"),value=NULL))})
      output$condPanels56 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 6 ", numericInput("upah6", labelMandatory("TOTAL UPAH 6"),value=NULL))})
      output$condPanels57 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 7 ", numericInput("upah7", labelMandatory("TOTAL UPAH 7"),value=NULL))})
      output$condPanels58 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("upah8", labelMandatory("TOTAL UPAH 8"),value=NULL))})
      
      output$condPanels61 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 1 ", numericInput("bpjs1", labelMandatory("TOTAL BPJS 1"),value=NULL))})
      output$condPanels62 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("bpjs2", labelMandatory("TOTAL BPJS 2"),value=NULL))})
      output$condPanels63 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("bpjs3", labelMandatory("TOTAL BPJS 3"),value=NULL))})
      output$condPanels64 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("bpjs4", labelMandatory("TOTAL BPJS 4"),value=NULL))})
      output$condPanels65 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("bpjs5", labelMandatory("TOTAL BPJS 5"),value=NULL))})
      output$condPanels66 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 6 ", numericInput("bpjs6", labelMandatory("TOTAL BPJS 6"),value=NULL))})
      output$condPanels67 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 7 ", numericInput("bpjs7", labelMandatory("TOTAL BPJS 7"),value=NULL))})
      output$condPanels68 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("bpjs8", labelMandatory("TOTAL BPJS 8"),value=NULL))})
      
      output$condPanels71 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 1 ", numericInput("tunjangan1", labelMandatory("TOTAL TUNJANGAN 1"),value=NULL))})
      output$condPanels72 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("tunjangan2", labelMandatory("TOTAL TUNJANGAN 2"),value=NULL))})
      output$condPanels73 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("tunjangan3", labelMandatory("TOTAL TUNJANGAN 3"),value=NULL))})
      output$condPanels74 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("tunjangan4", labelMandatory("TOTAL TUNJANGAN 4"),value=NULL))})
      output$condPanels75 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("tunjangan5", labelMandatory("TOTAL TUNJANGAN 5"),value=NULL))})
      output$condPanels76 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 6 ", numericInput("tunjangan6", labelMandatory("TOTAL TUNJANGAN 6"),value=NULL))})
      output$condPanels77 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 7 ", numericInput("tunjangan7", labelMandatory("TOTAL TUNJANGAN 7"),value=NULL))})
      output$condPanels78 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("tunjangan8", labelMandatory("TOTAL TUNJANGAN 8"),value=NULL))})
      
      output$condPanels81 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 1 ", numericInput("hi1", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 1"),value=NULL))})
      output$condPanels82 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("hi2", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 2"),value=NULL))})
      output$condPanels83 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("hi3", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 3"),value=NULL))})
      output$condPanels84 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("hi4", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 4"),value=NULL))})
      output$condPanels85 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("hi5", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 5"),value=NULL))})
      output$condPanels86 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 6 ", numericInput("hi6", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 6"),value=NULL))})
      output$condPanels87 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 7 ", numericInput("hi7", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 7"),value=NULL))})
      output$condPanels88 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("hi8", labelMandatory("TOTAL HUBUNGAN INDUSTRIAL 8"),value=NULL))})
      
      output$condPanels91 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 1 ", numericInput("perleng1", labelMandatory("TOTAL PERLENGKAPAN KERJA 1"),value=NULL))})
      output$condPanels92 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 2 ", numericInput("perleng2", labelMandatory("TOTAL PERLENGKAPAN KERJA 2"),value=NULL))})
      output$condPanels93 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 3 ", numericInput("perleng3", labelMandatory("TOTAL PERLENGKAPAN KERJA 3"),value=NULL))})
      output$condPanels94 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 4 ", numericInput("perleng4", labelMandatory("TOTAL PERLENGKAPAN KERJA 4"),value=NULL))})
      output$condPanels95 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 5 ", numericInput("perleng5", labelMandatory("TOTAL PERLENGKAPAN KERJA 5"),value=NULL))})
      output$condPanels96 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 6 ", numericInput("perleng6", labelMandatory("TOTAL PERLENGKAPAN KERJA 6"),value=NULL))})
      output$condPanels97 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 7 ", numericInput("perleng7", labelMandatory("TOTAL PERLENGKAPAN KERJA 7"),value=NULL))})
      output$condPanels98 <- renderUI({
        conditionalPanel(condition = "input.Kat >= 8 ", numericInput("perleng8", labelMandatory("TOTAL PERLENGKAPAN KERJA 8"),value=NULL))})
      
      
        to_be_done_at_submit <- eventReactive(input$submit, {          #Collect data
          cek_type1=input$cek_type1
          cek_type2=input$cek_type2
          cek_type3=input$cek_type3
          cek_type4=input$cek_type4
          cek_type5=input$cek_type5
          cek_type6=input$cek_type6
          cek_type7=input$cek_type7
          cek_type8=input$cek_type8
          biaya_peng1=input$biaya_peng1
          biaya_peng2=input$biaya_peng2
          biaya_peng3=input$biaya_peng3
          biaya_peng4=input$biaya_peng4
          biaya_peng5=input$biaya_peng5
          biaya_peng6=input$biaya_peng6
          biaya_peng7=input$biaya_peng7
          biaya_peng8=input$biaya_peng8
          man_fee1=input$man_fee1
          man_fee2=input$man_fee2
          man_fee3=input$man_fee3
          man_fee4=input$man_fee4
          man_fee5=input$man_fee5
          man_fee6=input$man_fee6
          man_fee7=input$man_fee7
          man_fee8=input$man_fee8
          tot1=biaya_peng1+man_fee1
          tot2=biaya_peng2+man_fee2
          tot3=biaya_peng3+man_fee3
          tot4=biaya_peng4+man_fee4
          tot5=biaya_peng5+man_fee5
          tot6=biaya_peng6+man_fee6
          tot7=biaya_peng7+man_fee7
          tot8=biaya_peng8+man_fee8
          tenaga_kerja1=input$tenaga_kerja1
          tenaga_kerja2=input$tenaga_kerja2
          tenaga_kerja3=input$tenaga_kerja3
          tenaga_kerja4=input$tenaga_kerja4
          tenaga_kerja5=input$tenaga_kerja5
          tenaga_kerja6=input$tenaga_kerja6
          tenaga_kerja7=input$tenaga_kerja7
          tenaga_kerja8=input$tenaga_kerja8
          upah1=input$upah1
          upah2=input$upah2
          upah3=input$upah3
          upah4=input$upah4
          upah5=input$upah5
          upah6=input$upah6
          upah7=input$upah7
          upah8=input$upah8
          bpjs1=input$bpjs1
          bpjs2=input$bpjs2
          bpjs3=input$bpjs3
          bpjs4=input$bpjs4
          bpjs5=input$bpjs5
          bpjs6=input$bpjs6
          bpjs7=input$bpjs7
          bpjs8=input$bpjs8
          tunjangan1=input$tunjangan1
          tunjangan2=input$tunjangan2
          tunjangan3=input$tunjangan3
          tunjangan4=input$tunjangan4
          tunjangan5=input$tunjangan5
          tunjangan6=input$tunjangan6
          tunjangan7=input$tunjangan7
          tunjangan8=input$tunjangan8
          hi1=input$hi1
          hi2=input$hi2
          hi3=input$hi3
          hi4=input$hi4
          hi5=input$hi5
          hi6=input$hi6
          hi7=input$hi7
          hi8=input$hi8
          perleng1=input$perleng1
          perleng2=input$perleng2
          perleng3=input$perleng3
          perleng4=input$perleng4
          perleng5=input$perleng5
          perleng6=input$perleng6
          perleng7=input$perleng7
          perleng8=input$perleng8
          
          if(input$os_type== "Alih Daya"|input$os_type== "Pemborongan")
          {noprs=input$no_prs
            nopds=input$no_pds
            nopdc=" "} 
          else if (input$os_type== "Project PDC") 
          {  nopdc=input$spnwr
          noprs=" "
          nopds=" "}
          else  
          {noprs=" "
          nopds=" "
          nopdc=" "}
          
          elapsed_months <- function(end_date, start_date) {
            ed <- as.POSIXlt(end_date)
            sd <- as.POSIXlt(start_date)
            12 * (ed$year - sd$year) + (ed$mon - sd$mon)}
          jgkwkt=elapsed_months(input$tgl_c, input$tgl_a)
          
          if (input$st_kon=="Aktif")
          {progres="Sudah Terbit Kontrak/PO"}
          else if (input$st_kon=="Non Aktif") {
            progres="Proses Perpanjangan/Penyusunan Kontrak"}
          else if (input$st_kon=="Putus") {
            progres="Kontrak Tidak Lanjut"}
          
          dt=matrix(data=NA,nrow =input$Kat,ncol=29)
          
  for (i in 1:input$Kat)
    {dt[i,] <- c(input$pic_type,
                 months(input$date1),
                 format(input$date2,"%Y"), 
                 input$reg_type,
                 input$kota,nopds,noprs,nopdc,
                 input$Nmprs,
                 input$Nmpkrj,
                 input$os_type, 
                 eval(as.symbol(paste("cek_type",i,sep=""))), 
                 eval(as.symbol(paste("tenaga_kerja",i,sep=""))), 
                 format(input$tgl_a, "%d/%m/%Y"),
                 format(input$tgl_b, "%d/%m/%Y"),
                 format(input$tgl_c, "%d/%m/%Y"),
                 jgkwkt, 
                 input$prog_pkj,
                 input$st_kon, 
                 eval(as.symbol(paste("upah",i,sep=""))),
                 eval(as.symbol(paste("bpjs",i,sep=""))),
                 eval(as.symbol(paste("tunjangan",i,sep=""))),
                 eval(as.symbol(paste("hi",i,sep=""))),
                 eval(as.symbol(paste("perleng",i,sep=""))),
                 eval(as.symbol(paste("biaya_peng",i,sep=""))), 
                 eval(as.symbol(paste("man_fee",i,sep=""))),
                 eval(as.symbol(paste("tot",i,sep=""))),
                 progres,
                 input$ket_prog
           )
          
          dtData=as.data.table(dt)}
          dtData[,3]=as.numeric(dtData[,3])
          dtData[,13]=as.numeric(dtData[,13])
          dtData[,14]=dmy(dtData[,14])
          dtData[,15]=dmy(dtData[,15])
          dtData[,16]=dmy(dtData[,16])
          dtData[,17]=as.numeric(dtData[,17])
          dtData[,18]=percent((as.numeric(dtData[,18])/100),2)
          
          dtData[,20]=as.numeric(dtData[,20])
          dtData[,21]=as.numeric(dtData[,21])
          dtData[,22]=as.numeric(dtData[,22])
          dtData[,23]=as.numeric(dtData[,23])
          dtData[,24]=as.numeric(dtData[,24])
          dtData[,25]=as.numeric(dtData[,25])
          dtData[,26]=as.numeric(dtData[,26])
          dtData[,27]=as.numeric(dtData[,27])
          
          #Put data on drive
          sheet_append(ss = sheet_id, data = dtData, sheet = "DATA")
          
          #Say thankyou
          h5("Thanks for entering data. (Terimakasih sudah memasukkan data. Anda bisa melihat dan mengunduh semua data ",
             a("di sini)", 
               href = "https://docs.google.com/spreadsheets/d/1nnm1ydjtt0YwyxuQXX8se-L3z9aMjBDdosdsF4GfEgM/edit#gid=0")
          )
        })
        
        output$thankyoutext <- renderUI({
          to_be_done_at_submit()  })
        
        observeEvent(input$submit, {
          to_be_done_at_submit()
          response<-paste0("Submited Success","!")
          showNotification(response,duration=0,type="message")
          resetForm(session)
          shinyjs::reset("form") })
    
      output$res <- renderDataTable({
        dtDatasofar <- vals$Data
        dtDatasofar[["Actions"]]<-
          paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Delete</button>
             </div>
             
             ')
        dtDatasofar$`PROGRESS PEKERJAAN (%)` <-  formattable::percent(dtDatasofar$`PROGRESS PEKERJAAN (%)`,digits = 2 )
        datatable(dtDatasofar,escape=F,editable="cell")
                 })
     
      df <- reactiveVal(as.data.table(read_sheet(ss =sheet_id,sheet = "DATA")))
      observeEvent(input[["table_cell_edit"]], {
        cell <- input[["table_cell_edit"]]
        newdf <- df
        newdf[cell$row, cell$col] <- cell$value
        df(newdf)
        sheet_write(df(newdf),ss = sheet_id,sheet = "DATA")
      })
      
      output$ris <- renderDataTable({
        dtDatasofar <- as.data.table(read_sheet(ss = sheet_id,sheet = "Ringkasan"))
        dt1=datatable(dtDatasofar)
        dt1%>%formatStyle('NAMA PEKERJAAN',target = 'row',background =  "white")
       })
      
        observeEvent(input$refresh, {reactive(input$res)
          sheet_write(df(),ss = sheet_id,sheet = "DATA")
          output$res <- renderDataTable({
            dtDatasofar <- vals$Data
            dtDatasofar[["Actions"]]<-
              paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Delete</button>
             </div>
             
             ')
            dtDatasofar$`PROGRESS PEKERJAAN (%)` <-  formattable::percent(dtDatasofar$`PROGRESS PEKERJAAN (%)`,digits = 2 )
            datatable(dtDatasofar,escape=F,editable="cell")
          })
                    })
        
        observeEvent(input$refresh1, {reactive(input$ris)
          output$ris <- renderDataTable({
            dtDatasofar <- datatable(read_sheet(ss = sheet_id,
        sheet = "Ringkasan"), options=list(iDisplayLength=10,bLengthChange=F))
            dtDatasofar%>%formatStyle('NAMA PEKERJAAN',target = 'row',background =  "white")})})
        
        observeEvent(input$clear, {
          resetForm(session)  })
        
        to_be_done_at_cek <- eventReactive(input$cek, {
          blnthn=data.table(input$bln1,input$thn1)
          range_write(ss = sheet_id, data = blnthn, sheet = "Sheet4",range="A1")})
        observeEvent(input$cek, { reactive(input$rkp)
          to_be_done_at_cek()
          output$rkp <- renderDataTable({
            dt=read_sheet(ss = sheet_id,sheet = "rkap")
            dt1 <- datatable(dt,rownames=FALSE, options=list(iDisplayLength=27,bLengthChange=F,autoWidth = FALSE,
                                                             columnDefs = list(list(className='dt-center', width = "200px", targets = "_all"))),class = 'cell-border stripe')
            dt1%>%formatStyle('PEKERJAAN',target = 'row',background =  "white")%>%
              formatStyle('PEKERJAAN',  target = 'row', fontWeight = "bold",  #(1,"bold")
                backgroundColor = styleEqual(c("Jumlah","TOTAL PENDAPATAN USAHA"), c('#ff9999', '#66a3ff')))%>%  
              formatStyle('PEKERJAAN',  target = 'row', backgroundColor = styleEqual(c("Pg","Lain-Lain",
                "Fee Manajemen Alih Daya", "PENDAPATAN ALIH DAYA","PP","Sewa Kendaraan"),
                c('#ffff','#ffff','#ffff','#ffff','#ffff','#ffff')))%>%
              formatStyle('PEKERJAAN',target = "row",fontWeight = styleEqual(c("Jumlah","TOTAL PENDAPATAN USAHA",
                "PENDAPATAN ALIH DAYA","PENDAPATAN PEMBORONGAN","PENDAPATAN PDC"),c("bold","bold","bold","bold","bold")))%>%
              formatPercentage(columns = c("TRW I (%)","TRW II (%)","TRW III (%)","TRW IV (%)","Persentase"),digits = 2 )
          })
          })
    
    output$rkp <- renderDataTable({
      dt=read_sheet(ss = sheet_id,sheet = "rkap")
      dt1 <- datatable(dt,rownames=FALSE, options=list(iDisplayLength=27,bLengthChange=F,autoWidth = FALSE,
    columnDefs = list(list(className='dt-center', width = "200px", targets = "_all"))),class = 'cell-border stripe')
      dt1%>%formatStyle('PEKERJAAN',target = 'row',background =  "white")%>%
        formatStyle('PEKERJAAN',  target = 'row', fontWeight = "bold",  #(1,"bold")
       backgroundColor = styleEqual(c("Jumlah","TOTAL PENDAPATAN USAHA"), c('#ff9999', '#66a3ff')))%>%  
        formatStyle('PEKERJAAN',  target = 'row', backgroundColor = styleEqual(c("Pg","Lain-Lain",
        "Fee Manajemen Alih Daya", "PENDAPATAN ALIH DAYA","PP","Sewa Kendaraan"),
        c('#ffff','#ffff','#ffff','#ffff','#ffff','#ffff')))%>%
        formatStyle('PEKERJAAN',target = "row",fontWeight = styleEqual(c("Jumlah","TOTAL PENDAPATAN USAHA",
                "PENDAPATAN ALIH DAYA","PENDAPATAN PEMBORONGAN","PENDAPATAN PDC"),c("bold","bold","bold","bold","bold")))%>%
       # formatCurrency(columns = c("RKAP TAHUN 2021","TOTAL/TAHUN"),"RP")%>% 
      formatPercentage(columns = c("TRW I (%)","TRW II (%)","TRW III (%)","TRW IV (%)","Persentase"),digits = 2 )
      })
 
    output[["downloadData"]] <- downloadHandler(
      filename = function() {
        "mydata.csv"
      },
      content = function(file) {
        dt=as.data.table(read_sheet(ss = sheet_id,sheet = "rkap"))
         dt <- sapply(dt, as.character)
        dt[is.na(dt)] <- " "
        dt=as.data.frame(dt)
        dt$`TRW I (%)`=percent(dt$`TRW I (%)`)
        dt$`TRW II (%)`=percent(dt$`TRW II (%)`)
        dt$`TRW III (%)`=percent(dt$`TRW III (%)`)
        dt$`TRW IV (%)`=percent(dt$`TRW IV (%)`)
        dt$Persentase=percent(dt$Persentase)
      }
    )
        output$picture<-renderUI({
          imgurl2 <- "https://i.ibb.co/nRPhwwC/logo-pds.png"
          div(id = "myImage", 
            tags$a(  href="https://www.ptpds.co.id/main/index.php",
              tags$img(src ="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAYYAAAC+CAYAAAAx3qiRAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAA3ppVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNi1jMTMyIDc5LjE1OTI4NCwgMjAxNi8wNC8xOS0xMzoxMzo0MCAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHhtbG5zOnN0UmVmPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3VyY2VSZWYjIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHhtcE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRpZDowYzAwZjYyZC0wY2E5LTQ4YTAtYmIzYi1hZTAyZjQ2ZTcyNDEiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6ODk4RTA5RjBDMzk5MTFFODhGNTNDMEUzRDU0NTdFRTQiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6ODk4RTA5RUZDMzk5MTFFODhGNTNDMEUzRDU0NTdFRTQiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIDIwMTUuNSAoTWFjaW50b3NoKSI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjBjMDBmNjJkLTBjYTktNDhhMC1iYjNiLWFlMDJmNDZlNzI0MSIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDowYzAwZjYyZC0wY2E5LTQ4YTAtYmIzYi1hZTAyZjQ2ZTcy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",
                       width = 150,title="PDS")
         ) )})
        output$picture1<-renderUI({
          imgurl1 <- "https://seeklogo.com/images/P/pt-pelabuhan-indonesia-iii-persero-logo-FC4E3B9D8C-seeklogo.com.png"
          div(id = "myImage",
              tags$img(src = imgurl1,width = 150)
          )})
        
        output$checktahun<-renderUI({
          dtDatasofar <- as.data.table(rs)
          checkboxGroupInput("thn","Tahun",choices = unique(dtDatasofar$TAHUN),selected = 2021)}) 
        
        output$checkbulan<-renderUI({
          dtDatasofar <- as.data.table(rs)
          checkboxGroupInput("bulan","Bulan",choices = unique(dtDatasofar$BULAN),selected = unique(dtDatasofar$BULAN))})
        output$checkpic<-renderUI({
          dtDatasofar <- as.data.table(rs)
          checkboxGroupInput("pic","Penanggung Jawab",choices = unique(dtDatasofar$PIC),selected = unique(dtDatasofar$PIC))})
        
        output$checkreg<-renderUI({
          dtDatasofar <- as.data.table(rs)
          checkboxGroupInput("reg","Regional",choices = unique(dtDatasofar$REGIONAL),selected = unique(dtDatasofar$REGIONAL))})
        
        output$checkperj<-renderUI({
          dtDatasofar <- as.data.table(rs)
          checkboxGroupInput("perj","Jenis Perjanjian",choices = unique(dtDatasofar$`JENIS PERJANJIAN`),selected = unique(dtDatasofar$`JENIS PERJANJIAN`))})
        
        output$checkkat<-renderUI({
          dtDatasofar <- as.data.table(rs)
          checkboxGroupInput("kat","Kategori Pekerjaan",choices = unique(dtDatasofar$`KATEGORI PEKERJAAN`),selected = unique(dtDatasofar$`KATEGORI PEKERJAAN`))})
        
  output$graf1performa <- renderPlot({
    data <- data.frame(read_sheet(ss = sheet_id,sheet = "DATA"))
    sum_dt <-aggregate(data$PENDAPATAN.USAHA,by=list(data$BULAN,data$TAHUN,data$REGIONAL),FUN=sum)
    colnames(sum_dt)=c("BULAN","TAHUN","REGIONAL","TOTAL")
    sum_dt$REGIONAL[sum_dt$REGIONAL=="Banyuwangi Bali Nusra Tenggara"] <- "BBN"
       #a=subset(subset(sum_dt,TAHUN==input$thn),BULAN==input$bulan|BULAN==input$bulan)
    #a=sum_dt %>% filter(BULAN == input$bulan)%>% filter(TAHUN==input$thn)
   a=sum_dt$BULAN %in% input$bulan
    b=sum_dt[a,]
    a1=b$TAHUN %in% input$thn
    b1=b[a1,]
    b1$TOTAL=round(b1$TOTAL/1000000000,digits=3)
    b2=aggregate(b1$TOTAL,by=list(b1$REGIONAL),FUN=sum)
    colnames(b2)=c("REGIONAL","TOTAL")
    
    ggplot(b2, aes(x= REGIONAL, y=TOTAL, fill=REGIONAL),ylab=) + geom_bar(stat="identity") +theme_minimal() + geom_text(aes(label=TOTAL),
        vjust=-0.5, color="black",position = position_dodge(0.9), size=4)+ylab("Total Pendapatan Usaha (Miliar Rupiah)")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) })
  
  output$graf1performa1 <- renderPlot({
    sum_dt <-aggregate(dz1$BIAYA.PENGELOLAAN,by=list(dz1$BULAN,dz1$TAHUN,dz1$REGIONAL),FUN=sum)
    colnames(sum_dt)=c("BULAN","TAHUN","REGIONAL","TOTAL")
    sum_dt$REGIONAL[sum_dt$REGIONAL=="Banyuwangi Bali Nusra Tenggara"] <- "BBN"
    a=sum_dt$BULAN %in% input$bulan
    b=sum_dt[a,]
    a1=b$TAHUN %in% input$thn
    b1=b[a1,]
    b1$TOTAL=round(b1$TOTAL/1000000000,digits=3)
    b2=aggregate(b1$TOTAL,by=list(b1$REGIONAL),FUN=sum)
    colnames(b2)=c("REGIONAL","TOTAL")
    
    ggplot(b2, aes(x= REGIONAL, y=TOTAL, fill=REGIONAL)) + geom_bar(stat="identity") +theme_minimal() + geom_text(aes(label=TOTAL),
      vjust=-0.5, color="black",position = position_dodge(0.9), size=4)+ylab("Total Biaya Pengelolaan (Miliar Rupiah)")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) })
  
  output$graf1performa2 <- renderPlot({
    sum_dt <-aggregate(dz1$MANAJEMEN.FEE ,by=list(dz1$BULAN,dz1$TAHUN,dz1$REGIONAL),FUN=sum)
    colnames(sum_dt)=c("BULAN","TAHUN","REGIONAL","TOTAL")
    sum_dt$REGIONAL[sum_dt$REGIONAL=="Banyuwangi Bali Nusra Tenggara"] <- "BBN"
      a=sum_dt$BULAN %in% input$bulan
    b=sum_dt[a,]
    a1=b$TAHUN %in% input$thn
    b1=b[a1,]
    b1$TOTAL=round(b1$TOTAL/100000000,digits=3)
    b2=aggregate(b1$TOTAL,by=list(b1$REGIONAL),FUN=sum)
    colnames(b2)=c("REGIONAL","TOTAL")
    
    ggplot(b2, aes(x= REGIONAL, y=TOTAL, fill=REGIONAL)) + geom_bar(stat="identity") +theme_minimal() + geom_text(aes(label=TOTAL),
  vjust=-0.5, color="black",position = position_dodge(0.9), size=4)+ylab("Total Manajemen Fee (Ratus Juta Rupiah)")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
  })

output$graf1performa3 <- renderPlot({
  sum_dt <-aggregate(dz1$MANAJEMEN.FEE ,by=list(dz1$BULAN,dz1$TAHUN,dz1$REGIONAL),FUN=sum)
  colnames(sum_dt)=c("BULAN","TAHUN","REGIONAL","TOTAL")
  a=sum_dt$BULAN %in% input$bulan
  b=sum_dt[a,]
  a1=b$TAHUN %in% input$thn
  b1=b[a1,]
  b1$TOTAL=round(b1$TOTAL/100000000,digits=3)
  b2=aggregate(b1$TOTAL,by=list(b1$REGIONAL),FUN=sum)
  colnames(b2)=c("REGIONAL","TOTAL")
  b2$persen=b2$TOTAL/sum(b2$TOTAL)*100
  
  slices <-b2$persen
  Regional <- c("BBN", "Jawa Tengah", "Jawa Timur", "Kalimantan", "Non Captive")
   label1=paste(round(slices,2),"%", sep = "")
  focus = c(0, 0.1, 0, 0, 0)
  start = c(0, 1, 2, 3, 4)
  end = c(1, 2, 3, 4, 2*pi)
  df=data.frame(slices,Regional,label1,focus,start,end)
  
  df <- df %>% 
    mutate(end = 2 * pi * cumsum(slices)/sum(slices),
           start = lag(end, default = 0),
           middle = 0.5 * (start + end),
           hjust = ifelse(middle > pi, 1, 0),
           vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  
  ggplot(df) + geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, amount = slices, fill = Regional, explode = focus),
        stat = 'pie',color = "transparent") + scale_fill_brewer('', palette = 'Set1') + coord_fixed()+theme_minimal()+
    theme_no_axes()+ geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label1, hjust = hjust, vjust = vjust),size=5)+
    theme(panel.border = element_blank(),panel.background = element_blank()) 
  })

output$graf1performa4 <- renderPlot({
  sum_dt <-aggregate(dz1$PENDAPATAN.USAHA,by=list(dz1$BULAN,dz1$TAHUN,dz1$REGIONAL),FUN=sum)
  colnames(sum_dt)=c("BULAN","TAHUN","REGIONAL","TOTAL")
  sum_dt$REGIONAL[sum_dt$REGIONAL=="Banyuwangi Bali Nusra Tenggara"] <- "BBN"
  a1=sum_dt$TAHUN %in% input$thn
  b1=sum_dt[a1,]
  b1$TOTAL=round(b1$TOTAL/1000000000,digits=3)
  b2=aggregate(b1$TOTAL,by=list(b1$BULAN),FUN=sum)
  colnames(b2)=c("BULAN","TOTAL")
  b2$kat="realisasi"
  b2=b2[-10,]
  data1 <- data.frame(read_sheet(ss = sheet_id,sheet = "Sheet7"))
  data1$kat="target"
  df=rbind(b2,data1)
  df$BULAN=as.factor(df$BULAN)
  colnames(df)=c("BULAN","TOTAL","KATEGORI")
  level_order <- c('January', 'February','March','April','May','June','July','August','September','October','November','December')
  
  ggplot(df,aes(x =factor(BULAN, level = level_order),y = TOTAL,group = KATEGORI,color=KATEGORI))+ xlab("BULAN")+ylab("Pendapatan Usaha (Miliar Rupiah)")+
    geom_line(size=1) + geom_point(aes(shape=KATEGORI))+ scale_color_manual(values=c('#0000ff','#ff0000'))+theme_minimal()+
    theme(panel.grid.major = element_blank(), axis.line = element_line(colour = "black")) 
  })

output$progressBox <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  count_dt <- dt %>% count(TAHUN)
  a=subset(count_dt,TAHUN==input$thn)
  valueBox(paste0(a$n), "TOTAL PEKERJAAN", icon = icon("briefcase"),color = "orange")})
output$tot_pen <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  sum_dt <-aggregate(dt$TOTAL.PENDAPATAN.USAHA,by=list(dt$TAHUN),FUN=sum)
  colnames(sum_dt)=c("TAHUN","tOTAL")
  a=subset(sum_dt,TAHUN==input$thn)
  b=a$tOTAL/1000000000
  valueBox(paste0(round(b,3), " M"), "TOTAL PENDAPATAN USAHA", icon = icon("hand-holding-usd"),color = "red")})
output$tot_bi <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  sum_dt <-aggregate(dt$TOTAL.BIAYA.PENGELOLAAN,by=list(dt$TAHUN),FUN=sum)
  colnames(sum_dt)=c("TAHUN","tOTAL")
  a=subset(sum_dt,TAHUN==input$thn)
  b=a$tOTAL/1000000000
  valueBox(paste0(round(b,3), " M"), "TOTAL BIAYA PENGELOLAAN", icon = icon("coins"),color = "purple")})
output$tot_man <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  sum_dt <-aggregate(dt$TOTAL.MANAJEMEN.FEE,by=list(dt$TAHUN),FUN=sum)
  colnames(sum_dt)=c("TAHUN","tOTAL")
  a=subset(sum_dt,TAHUN==input$thn)
  b=a$tOTAL/1000000000
  valueBox(paste0(round(b,3), " M"), "TOTAL MANAJEMEN FEE", icon = icon("file-invoice-dollar"),color = "teal")
})
output$jum_pem <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  count_dt <- dt %>% count(TAHUN,JENIS.PERJANJIAN)
  a=subset(subset(count_dt,TAHUN==input$thn),JENIS.PERJANJIAN=="Pemborongan")
  valueBox(paste0(a$n), "TOTAL PEMBORONGAN", icon = icon("user-tie"),color = "yellow")})
output$jum_ad <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  count_dt <- dt %>% count(TAHUN,JENIS.PERJANJIAN)
  a=subset(subset(count_dt,TAHUN==input$thn),JENIS.PERJANJIAN=="Alih Daya")
  valueBox(paste0(a$n), "TOTAL ALIH DAYA", icon = icon("user-friends"),color = "green")})
output$jum_pdc <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  count_dt <- dt %>% count(TAHUN,JENIS.PERJANJIAN)
  a=subset(subset(count_dt,TAHUN==input$thn),JENIS.PERJANJIAN=="Project PDC")
  valueBox(paste0(a$n), "TOTAL PROJECT PDC", icon = icon("user-graduate"),color = "fuchsia")
})
output$jum_can <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  count_dt <- dt %>% count(TAHUN,JENIS.PERJANJIAN)
  a=subset(subset(count_dt,TAHUN==input$thn),JENIS.PERJANJIAN=="Canvasing")
  valueBox(paste0(a$n), "TOTAL CANVASING", icon = icon("user-alt"),color = "aqua")})
output$stat_akt <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  count_dt <- dt %>% count(TAHUN,STATUS.KONTRAK)
  a=subset(subset(count_dt,TAHUN==input$thn),STATUS.KONTRAK=="Aktif")
  valueBox(paste0(a$n), "KONTRAK AKTIF", icon = icon("check-square"),color = "light-blue")})
output$stat_non <- renderValueBox({
  dt<- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan" ))
  count_dt <- dt %>% count(TAHUN,STATUS.KONTRAK)
  a=subset(subset(count_dt,TAHUN==input$thn),STATUS.KONTRAK=="Non Aktif" | STATUS.KONTRAK=="Putus")
  valueBox(paste0(sum(a$n)), "KONTRAK NONAKTIF/ PUTUS", icon = icon("times-circle"),color = "olive")})

observeEvent(input$refresh2, {
  dtDatasofar <- as.data.table(read_sheet(ss = sheet_id,sheet = "DATA"))
  reactive(input$checktahun)
output$checktahun<-renderUI({
    checkboxGroupInput("thn","Tahun",choices = unique(dtDatasofar$TAHUN),selected = unique(dtDatasofar$TAHUN))})
reactive(input$checkbulan)
output$checkbulan<-renderUI({

  checkboxGroupInput("bulan","Bulan",choices = unique(dtDatasofar$BULAN),selected = unique(dtDatasofar$BULAN))})
reactive(input$checkpic)
output$checkpic<-renderUI({
  checkboxGroupInput("pic","Penanggung Jawab",choices = unique(dtDatasofar$PIC),selected = unique(dtDatasofar$PIC))})
reactive(input$checkreg)
output$checkreg<-renderUI({
   checkboxGroupInput("reg","Regional",choices = unique(dtDatasofar$REGIONAL),selected = unique(dtDatasofar$REGIONAL))})
reactive(input$checkperj)
output$checkperj<-renderUI({
  checkboxGroupInput("perj","Jenis Perjanjian",choices = unique(dtDatasofar$`JENIS PERJANJIAN`),selected = unique(dtDatasofar$`JENIS PERJANJIAN`))})
reactive(input$checkkat)
output$checkkat<-renderUI({
   checkboxGroupInput("kat","Kategori Pekerjaan",choices = unique(dtDatasofar$`KATEGORI PEKERJAAN`),selected = unique(dtDatasofar$`KATEGORI PEKERJAAN`))})
})

output$graf21 <- renderPlot({
  data <- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan"))
  sum_dt <-aggregate(list(data$TOTAL.BIAYA.PENGELOLAAN, data$TOTAL.MANAJEMEN.FEE), by=list(data$REGIONAL,data$JENIS.PERJANJIAN),FUN=sum)
  colnames(sum_dt)=c("REGIONAL","Perjanjian","Biaya","Manfee")
  a=sum_dt$REGIONAL %in% input$reg
  b=sum_dt[a,]
  b2=aggregate(list(b$Biaya, b$Manfee),by=list(b$Perjanjian),FUN=sum)
  colnames(b2)=c("Perjanjian","Biaya","Manfee")
  b3=b2[,-3]
  b3$kat="Biaya Pengelolaan"
  colnames(b3)=c("Perjanjian","Total", "Kategori")
  b4=b2[,-2]
  b4$kat="Manajemen Fee"
  colnames(b4)=c("Perjanjian","Total", "Kategori")
  df=rbind(b3,b4)
  df$Total=round(df$Total/1000000000,digits=3)
  ggplot(df, aes(x = Perjanjian, y = Total , fill =Kategori ))+geom_col(position = "dodge") +theme_minimal() + geom_text(aes(label=Total),
  vjust=-0.5, color="black",position = position_dodge(0.9), size=4)+ylab("Miliar Rupiah")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
  })

output$graf12 <- renderPlot({
  sum_dt <-aggregate(dz1$PENDAPATAN.USAHA,by=list(dz1$BULAN,dz1$TAHUN,dz1$REGIONAL,dz1$PIC),FUN=sum)
  colnames(sum_dt)=c("BULAN","TAHUN","REGIONAL","PIC", "TOTAL")
  sum_dt$REGIONAL[sum_dt$REGIONAL=="Banyuwangi Bali Nusra Tenggara"] <- "BBN"
    a=sum_dt$BULAN %in% input$bulan
  b=sum_dt[a,]
  a1=b$TAHUN %in% input$thn
  b1=b[a1,]
  b1$TOTAL=round(b1$TOTAL/1000000000,digits=2)
  b2=aggregate(b1$TOTAL,by=list(b1$REGIONAL,b1$PIC),FUN=sum)
  colnames(b2)=c("REGIONAL","PIC" ,"TOTAL")
  ggplot(b2, aes(x = PIC, y = TOTAL , fill = REGIONAL))+geom_col(position = "dodge") +theme_minimal() + geom_text(aes(label=TOTAL),
vjust=-0.5, color="black",position = position_dodge(0.9), size=3.5)+ylab("Total Pendapatan Usaha (Miliar Rupiah)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  })

output$graf51 <- renderPlot({
  data <- data.frame(read_sheet(ss = sheet_id,sheet = "kat"))
  sum_dt <-aggregate(list(data$TOTAL.BIAYA.PENGELOLAAN, data$TOTAL.MANAJEMEN.FEE), by=list(data$KATEGORI.PEKERJAAN,data$PIC),FUN=sum)
  colnames(sum_dt)=c("KATEGORI","PIC","Biaya","Manfee")
  a=sum_dt$KATEGORI %in% input$kat
  b=sum_dt[a,]
  b2=aggregate(list(b$Biaya, b$Manfee),by=list(b$PIC),FUN=sum)
  colnames(b2)=c("PIC","Biaya","Manfee")
  b3=b2[,-3]
  b3$kat="Biaya Pengelolaan"
  colnames(b3)=c("PIC","Total", "Kategori")
  b4=b2[,-2]
  b4$kat="Manajemen Fee"
  colnames(b4)=c("PIC","Total", "Kategori")
  df=rbind(b3,b4)
  df$Total=round(df$Total/1000000000,digits=3)
  ggplot(df, aes(x = PIC, y = Total , fill =Kategori ))+geom_col(position = "dodge") +theme_minimal() + geom_text(aes(label=Total),
    vjust=-0.5, color="black",position = position_dodge(0.9), size=4)+ylab("Miliar Rupiah")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
})

output$graf31 <- renderPlot({
  data <- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan"))
  sum_dt <-aggregate(data$TOTAL.PENDAPATAN.USAHA,by=list(data$PIC, data$JENIS.PERJANJIAN, data$REGIONAL ),FUN=sum)
  colnames(sum_dt)=c("PIC","PERJANJIAN","REGIONAL", "TOTAL")
  sum_dt$REGIONAL[sum_dt$REGIONAL=="Banyuwangi Bali Nusra Tenggara"] <- "BBN"
  a=sum_dt$PIC %in% input$pic
  b=sum_dt[a,]
  b$TOTAL=round(b$TOTAL/1000000000,digits=2)
  b2=aggregate(b$TOTAL,by=list(b$REGIONAL,b$PERJANJIAN),FUN=sum)
  colnames(b2)=c("REGIONAL","PERJANJIAN" ,"TOTAL")
  ggplot(b2, aes(x = PERJANJIAN, y = TOTAL , fill = REGIONAL))+geom_col(position = "dodge") +theme_minimal() + geom_text(aes(label=TOTAL),
    vjust=-0.5, color="black",position = position_dodge(0.9), size=3.5)+ylab("Total Pendapatan Usaha (Miliar Rupiah)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  
  })

output$graf41 <- renderPlot({
  data <- data.frame(read_sheet(ss = sheet_id,sheet = "Ringkasan"))
  sum_dt <-aggregate(data$TOTAL.PENDAPATAN.USAHA,by=list(data$PIC, data$JENIS.PERJANJIAN, data$REGIONAL ),FUN=sum)
  colnames(sum_dt)=c("PIC","PERJANJIAN","REGIONAL", "TOTAL")
  sum_dt$REGIONAL[sum_dt$REGIONAL=="Banyuwangi Bali Nusra Tenggara"] <- "BBN"
  a=sum_dt$PERJANJIAN %in% input$perj
  b=sum_dt[a,]
  
  b$TOTAL=round(b$TOTAL/1000000000,digits=2)
  b2=aggregate(b$TOTAL,by=list(b$REGIONAL,b$PIC),FUN=sum)
  colnames(b2)=c("REGIONAL","PIC" ,"TOTAL")
  ggplot(b2, aes(x = PIC, y = TOTAL , fill = REGIONAL))+geom_col(position = "dodge") +theme_minimal() + geom_text(aes(label=TOTAL),
   vjust=-0.5, color="black",position = position_dodge(0.9), size=3.5)+ylab("Total Pendapatan Usaha (Miliar Rupiah)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  
})

output$graf11 <- renderImage({
  sum_dt <-aggregate(dz1$PENDAPATAN.USAHA,by=list(dz1$BULAN,dz1$TAHUN,dz1$PIC),FUN=sum)
  colnames(sum_dt)=c("BULAN","TAHUN","PIC","TOTAL")
  a1=sum_dt$TAHUN %in% input$thn
  b1=sum_dt[a1,]
  b1$TOTAL=round(b1$TOTAL/1000000000,digits=3)
 
 a= ggplot(b1,aes(x = PIC, y = TOTAL, group = BULAN,fill=PIC)) +geom_bar(stat="identity") +theme_minimal() + geom_text(aes(label=TOTAL),
    vjust=-0.5, color="black",position = position_dodge(0.9))+ylab("Pendapatan (Miliar Rupiah)")+ ylim(0,25)+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    transition_states(BULAN) +  ease_aes(y = 'bounce-out') + ggtitle('Bulan {closest_state}')+
   scale_fill_brewer(palette ="Oranges")+theme(legend.position = "none")
  
  outfile <- tempfile(fileext='.gif')
  anim_save("outfile.gif", animate(a, height = 2.7,width = 4.4, units = "in", res = 150))
  list(src = "outfile.gif",
       contentType = 'image/gif'
       # width = 400,
       # height = 300,
       # alt = "This is alternate text"
  )}, deleteFile = TRUE)

observeEvent(input$lastClick,
             {
               if (input$lastClickId%like%"delete")
               {
                 row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                 vals$Data=vals$Data[-row_to_del]
                 sheet_write(vals$Data,ss = sheet_id,sheet = "DATA")
               }
               else if (input$lastClickId%like%"modify")
               {
                 showModal(modal_modify)
               }
             }
)


})