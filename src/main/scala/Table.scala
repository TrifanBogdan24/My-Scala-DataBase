type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = {
    val headers = header.mkString(",") // Convertim lista de nume de coloane intr-un sir CSV
    val rows = tableData.map(_.values.mkString(",")) // Convertim fiecare rând într-un șir CSV
    (headers :: rows).mkString("\n") // Concatenăm numele coloanelor cu rândurile și folosim "\n" pentru a separa liniile
  }

  def insert(row: Row): Table = {
    val exists = tableData.exists(_ == row) // Verificăm dacă rândul există deja în tabel
    if (!exists) {
      val updatedData = tableData :+ row // Adăugăm rândul doar dacă nu există deja
      this.copy(tableData = updatedData) // Creăm o nouă instanță a tabelului cu lista de date actualizată
    } else {
      this // Dacă rândul există deja, returnăm baza de date nemodificată
    }
  }

  def delete(row: Row): Table = {
    val updatedData = tableData.filterNot(_ == row) // Filtrăm rândurile care nu sunt egale cu rândul dat ca parametru
    this.copy(tableData = updatedData) // Creăm o nouă instanță a tabelului cu lista de date actualizată
  }

  def sort(column: String): Table = {
    val sortedData = tableData.sortBy(_.getOrElse(column, "")) // Sortăm lista de date în funcție de valoarea din coloana specificată
    this.copy(tableData = sortedData) // Creăm o nouă instanță a tabelului cu lista de date sortată
  }

  def update(f: FilterCond, updates: Map[String, String]): Table = {
    // Mapăm fiecare rând din tabel și actualizăm valorile corespunzătoare dacă rândul îndeplinește condiția
    val updatedData = tableData.map { row =>
      if (f.eval(row).getOrElse(false)) {
        val updatedRow = row ++ updates // Adăugăm noile valori la rândul curent
        updatedRow
      } else {
        row // Dacă rândul nu îndeplinește condiția, îl returnăm nemodificat
      }
    }
    // Creăm un nou tabel cu datele actualizate și returnăm rezultatul
    Table(tableName, updatedData)
  }


  def filter(f: FilterCond): Table = {
    val filteredData = tableData.filter(row => f.eval(row).getOrElse(false))
    Table(tableName, filteredData)
  }

  def select(columns: List[String]): Table = {
    val selectedData = tableData.map(row => columns.map(col => col -> row.getOrElse(col, "")).toMap)
    Table(tableName, selectedData)
  }

  def header: List[String] = tableData.headOption.map(_.keys.toList).getOrElse(List.empty)
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val lines = s.split("\n")
    if (lines.length < 2) {
      throw new IllegalArgumentException("Invalid CSV format: must have at least one header line and one data line")
    }

    val headers = lines.head.split(",").toList
    val data = lines.tail.map(_.split(",").toList).map(row => headers.zip(row).toMap)
    val tabularData: Tabular = data.toList // Convertim datele în tipul Tabular
    new Table(name, tabularData)
  }
}

extension (table: Table) {
  def apply(i: Int): Table = {
    Table(table.tableName, List(table.tableData(i)))
  }
}
