type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = {
    val csvHeader = header.mkString(",")
    val csvData = tableData.map(row => row.values.mkString(",")).mkString("\n")
    s"$csvHeader\n$csvData"
  }

  def insert(row: Row): Table = {
    val exists = tableData.exists(_ == row) // Verificam daca randul exista deja în tabel
    if (!exists) {
      val updatedData = tableData :+ row // Adaugam randul doar daca nu exista deja
      this.copy(tableData = updatedData) // Cream o noua instanta a tabelului cu lista de date actualizata
    } else {
      this // Daca randul exista deja, returnam baza de date nemodificata
    }
  }

  def delete(row: Row): Table = {
    Table(tableName, tableData.filterNot(tableRow => tableRow == row))
  }

  def sort(column: String): Table = {
    val sortedData = tableData.sortBy(col => col.getOrElse(column, ""))
    Table(tableName, sortedData)
  }

  def update(f: FilterCond, updates: Map[String, String]): Table = {
    val updatedData = tableData.map { row =>
      if (f.eval(row).getOrElse(false)) {
        val updatedRow = row ++ updates
        updatedRow
      } else {
        row
      }
    }
    Table(tableName, updatedData)
  }

  def filter(f: FilterCond): Table = {
    val filteredData = tableData.filter { row =>
      f.eval(row).getOrElse(false)
    }
    Table(tableName, filteredData)
  }


  def select(columns: List[String]): Table = {
    val selectedData = tableData.map { row =>
      columns.map(col => col -> row.getOrElse(col, "")).toMap
    }
    Table(tableName, selectedData)
  }

  def header: List[String] = {
    tableData.headOption.map(_.keys.toList).getOrElse(List.empty)
  }

  def data: Tabular =
    tableData
  def name: String =
    tableName
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
    // pentru a accesa coloanele prin indecsi `List....`:
    Table(table.tableName, List(table.tableData(i)))
  }
}
