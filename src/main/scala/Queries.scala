object Queries {


  def killJackSparrow(t: Table): Option[Table] = {
    val condition: FilterCond = Field("name", name => name != "Jack")
    val filteredData = t.filter(condition)
    Some(filteredData)
  }

  def insertLinesThenSort(db: Database): Option[Table] = {
    // Creăm tabela "Inserted Fellas" și inserăm datele specificate
    val insertedTable = Table("Inserted Fellas", List(
      Map("name" -> "Ana", "age" -> "93", "CNP" -> "455550555"),
      Map("name" -> "Diana", "age" -> "33", "CNP" -> "255532142"),
      Map("name" -> "Tatiana", "age" -> "55", "CNP" -> "655532132"),
      Map("name" -> "Rosmaria", "age" -> "12", "CNP" -> "855532172")
    ))

    // Sortăm tabela după vârstă și returnăm rezultatul
    Some(insertedTable.sort("age"))
  }


  def youngAdultHobbiesJ(db: Database): Option[Table] = {
    val peopleTable = db.tables.find(_.tableName == "People")
    val hobbiesTable = db.tables.find(_.tableName == "Hobbies")

    (peopleTable, hobbiesTable) match {
      case (Some(people), Some(hobbies)) =>
        val filteredPeople = people.tableData.filter(row => row("name").startsWith("J") && row("age").toInt < 25)
        val joinedData = filteredPeople.flatMap { person =>
          val personName = person("name")
          val matchingHobbies = hobbies.tableData.filter(_("name") == personName)
          matchingHobbies.map(hobby => Map("name" -> personName, "hobby" -> hobby("hobby")))
        }
        Some(Table("YoungAdultHobbies", joinedData))
      case _ => None
    }
  }

}
