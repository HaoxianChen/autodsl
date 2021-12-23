package synthesis

import synthesis.activelearning.{ExampleInstance, TupleInstance}

case class Examples(elems: Map[Relation, Set[List[Constant]]]) {
  def addTuples(relation: Relation, tuples: Set[List[Constant]]): Examples = {
    val newMap = {
      val oldValue = elems.get(relation)
      val newValue: Set[List[Constant]] = if (oldValue.isDefined) oldValue.get ++ tuples else tuples
      elems + (relation -> newValue)
    }
    Examples(newMap)
  }

  def addTuples(tuples: Set[Tuple]) : Examples= {
    val tuplesByRel = tuples.groupBy(_.relation)

    var newExamples = this
    for ((rel, tuples) <- tuplesByRel) {
      newExamples = newExamples.addTuples(rel, tuples.map(_.fields))
    }
    newExamples
  }

  def toTuples(): Set[Tuple] = {
    def toTuples(relation: Relation, tuples: Set[List[Constant]]): Set[Tuple] = tuples.map(fields => Tuple(relation, fields))
    elems.flatMap {
      case (rel, _tuples) => toTuples(rel, _tuples)
    }.toSet
  }

  def toInstances(rel: Relation): Set[TupleInstance] = {
    if (elems.contains(rel)) {
      val tuples = elems(rel)
      val instanceIdType: Type = NumberType("InstanceId")
      val idx: Int = rel.signature.indexOf(instanceIdType)
      var allInstances: Set[TupleInstance] = Set()
      for ((i, tupleGroup) <- tuples.groupBy(t => t(idx))) {
        val _tupleGroup: Set[Tuple] = tupleGroup.map(t => Tuple(rel, t))
        val _i = i.name.toInt
        val instance = activelearning.TupleInstance(_tupleGroup, _i)
        allInstances += instance
      }
      allInstances
    }
    else {
      Set()
    }
  }

  def addInstance(tupleInstance: TupleInstance): Examples = ???
  def addInstances(tupleInstanceSet: Set[TupleInstance]): Examples = tupleInstanceSet.foldLeft(this)((e,t) => e.addInstance(t))

  def toFileStr(relation: Relation): String = {
    val facts = elems(relation)
    facts.map(l => l.mkString("\t")).mkString("\n")
  }

  def getConstantSet: Set[Constant] = {
    def _getConstants(constantListSet: Set[List[Constant]]): Set[Constant] = constantListSet.flatMap(_.toSet)

    var constants: Set[Constant] = Set()
    for ((_, v) <- elems) {
      constants ++= _getConstants(v)
    }
    constants
  }

  def filterByRelation(rel: Relation): Examples = new Examples(Map(rel->elems(rel)))
}
object Examples {
  def apply(): Examples = new Examples(Map())
  def empty(): Examples = new Examples(Map())


  def strToTuple(rel: Relation, str: List[String]): List[Constant] = {
    require(rel.signature.size == str.size, s"${rel}, ${str}, szie: ${str.size}")
    val fields: List[Constant] = for ((_type, s) <- rel.signature zip str) yield Constant(s, _type)
    fields
  }
}

case class Problem(name: String, domain: String, types: Set[Type], inputRels: Set[Relation], outputRels: Set[Relation],
                   edb: Examples , idb: Examples, oracleSpec: Option[String]) {

  private val typeMap: Map[String,Type] = (for (t<-types) yield t.name -> t).toMap

  def addType(_type: Type): Problem = {
    val newTypes = types + _type
    this.copy(types=newTypes)
  }
  def addInputRelation(relation: Relation): Problem = {
    val newRels  = inputRels + relation
    this.copy(inputRels=newRels)
  }
  def addInputRelation(relName: String, typeNames: List[String]): Problem = {
    val types: List[Type] = typeNames.map(typeMap(_))
    val relation: Relation = Relation(relName, types)
    addInputRelation(relation)
  }
  def addOutputRelation(relation: Relation): Problem = {
    val newRels  = outputRels + relation
    this.copy(outputRels=newRels)
  }
  def addOutputRelation(relName: String, typeNames: List[String]): Problem = {
    val types: List[Type] = typeNames.map(typeMap(_))
    val relation: Relation = Relation(relName, types)
    addOutputRelation(relation)
  }

  def addEdb(relation: Relation, facts: List[List[String]]): Problem = {
    require(inputRels.contains(relation))
    val newTuples = facts.map(Examples.strToTuple(relation, _)).toSet
    this.copy(edb=edb.addTuples(relation, newTuples))
  }
  def addIdb(relation: Relation, facts: List[List[String]]): Problem = {
    require(outputRels.contains(relation))
    val newTuples = facts.map(Examples.strToTuple(relation, _)).toSet
    this.copy(idb=idb.addTuples(relation, newTuples))
  }

  def addEdb(tuples: Set[Tuple]): Problem = {
    val newEdb = edb.addTuples(tuples)
    this.copy(edb=newEdb)
  }
  def addIdb(tuples: Set[Tuple]): Problem = {
    val newIdb = idb.addTuples(tuples)
    this.copy(idb=newIdb)
  }

  def getNumExampleInstances: Int = {
    if (types.contains(NumberType(s"InstanceId"))) {
      ExampleInstance.fromEdbIdb(edb,idb).size
    }
    else 1
  }

  def addOracleSpec(oracleSpec: String): Problem = this.copy(oracleSpec=Some(oracleSpec))


  def rename(newName: String): Problem = this.copy(name=newName)
  def addDomain(domain: String): Problem = this.copy(domain=domain)

  def getType(name: String): Option[Type] = typeMap.get(name)
  def inputTypes: Set[Type] = inputRels.flatMap(_.signature)
  def outputTypes: Set[Type] = outputRels.flatMap(_.signature)
}

object Problem {
  def apply(): Problem = Problem("new_problem", "new_domain",Set(), Set(), Set(), Examples(), Examples(), None)
}