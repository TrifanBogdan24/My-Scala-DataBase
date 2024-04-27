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
    // Realizăm join între tabelele "People" și "Hobbies" pe baza coloanei "name"
    val joinedTableOpt = db.join("People", "name", "Hobbies", "name")

    joinedTableOpt.map { joinedTable =>
      // Definim condițiile pentru filtrare
      val ageUnder25 = Field("age", _.toInt < 25)
      val startsWithJ = Field("name", _.startsWith("J"))
      val hasHobby = Field("hobby", _.nonEmpty)

      // Aplicăm condițiile pentru a filtra tabela
      val filteredTable = joinedTable.filter(
        And(
          And(ageUnder25, startsWithJ),
          hasHobby
        )
      )

      // Extragem doar coloanele "name" și "hobby"
      filteredTable.select(List("name", "hobby"))
    }
  }


}
