type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = {
    val csvHeader = header.mkString(", ")
    val csvRows = tableData.map(_.values.mkString(",")).mkString("\n")
    s"$csvHeader\n$csvRows"
  }

  def insert(row: Row): Table = {
    Table(tableName, tableData :+ row)
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
      val newRow = columns.map(col => col -> row.getOrElse(col, ""))
      newRow.toMap
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
    val rows = s.split("\n").toList.map { line =>
      val values = line.split(",").map(_.trim)
      val headers = values.headOption.map(_.split("\t").map(_.trim).toList).getOrElse(List.empty)
      val rowValues = values.drop(1)
      headers.zip(rowValues).toMap
    }
    new Table(name, rows)
  }
}



extension (table: Table) {
  // Implement indexing here, find the right function to override
  def apply(i: Int): Table = {
    Table(table.tableName, List(table.tableData(i)))
  }
}

