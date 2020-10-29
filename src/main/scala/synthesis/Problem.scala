package synthesis

case class Problem(types: Set[Type], inputRels: Set[Relation], outputRels: Set[Relation],
                   edb: Set[Tuple] , idb: Set[Tuple]) {

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
  def addEdb(tuple: Tuple): Problem = {
    val newEdb  = edb + tuple
    this.copy(edb=newEdb)
  }
  def addIdb(tuple: Tuple): Problem = {
    val newIdb  = idb + tuple
    this.copy(idb=newIdb)
  }

  def strToTuple(rel: Relation, str: List[String]): Tuple = {
    require(rel.signature.size == str.size, s"${rel}, ${str}, szie: ${str.size}")
    val fields: List[Constant] = for ((_type, s) <- rel.signature zip str) yield Constant(s, _type)
    Tuple(rel, fields)
  }
  def addEdb(relation: Relation, facts: List[List[String]]): Problem = {
    require(inputRels.contains(relation))
    val newTuples = facts.map(strToTuple(relation, _))
    this.copy(edb=edb++newTuples)
  }
  def addIdb(relation: Relation, facts: List[List[String]]): Problem = {
    require(outputRels.contains(relation))
    val newTuples = facts.map(strToTuple(relation,_))
    this.copy(idb=idb++newTuples)
  }

  def getType(name: String): Option[Type] = typeMap.get(name)
}

object Problem {
  def apply(): Problem = Problem(Set(), Set(), Set(), Set(), Set())
}