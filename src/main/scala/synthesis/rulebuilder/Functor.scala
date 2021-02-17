package synthesis.rulebuilder

import synthesis._

/** Functors */
sealed abstract class AbstractFunctorSpec {
  def name: String
  def signature: List[Type]

  def literalToString(literal: Literal): String
  def makeLiteral(inputs: List[Parameter], output: Parameter): FunctorLiteral

  def getRelation: Relation = Relation(name, signature)
}
object AbstractFunctorSpec {
  def isListType(_type: Type): Boolean = _type.name.endsWith("List")
  def getNodeName(_type: Type): String = {
    require(isListType(_type))
    _type.name.take(_type.name.size-4)
  }
}

case class FunctorLiteral(relation: Relation, fields: List[Parameter],
                          abstractFunctorSpec: AbstractFunctorSpec) extends Literal {
  override def toString: String = abstractFunctorSpec.literalToString(this)

  def rename(binding: Map[Parameter, Parameter]): Literal = {
    val newFields = _rename(binding)
    this.copy(fields=newFields)
  }

  def renameRelation(newRels: Map[Relation, Relation]): Literal = {
    val newRel = newRels.getOrElse(this.relation, this.relation)
    this.copy(relation=newRel)
  }
}

abstract class FunctorSpec() extends AbstractFunctorSpec {
  def inputIndices: List[Int]
  def outputIndex: Int

  /** no out of bound indices */
  require(inputIndices.forall(i => i < signature.size))
  require(outputIndex < signature.size)
  /** no duplicated indices */
  require((inputIndices :+ outputIndex).toSet.size == inputIndices.size+1)
  /** length of signature is the same as input + output */
  require(inputIndices.size + 1 == signature.size)

  def inputSig: List[Type] = inputIndices.map(i => signature(i))
  def outputSig: Type = signature(outputIndex)

  def getInputs(literal: Literal): List[Parameter] = inputIndices.map(i => literal.fields(i))
  def getOutput(literal: Literal): Parameter = literal.fields(outputIndex)

  def makeLiteral(inputs: List[Parameter], output: Parameter): FunctorLiteral = {
    val fields: List[Parameter] = signature.zipWithIndex.map { case (t, i) => {
      val p: Parameter = if (inputIndices.contains(i)) {
        val idx = inputIndices.indexOf(i)
        inputs(idx)
        }
        else output
      assert(p._type == t)
      p
      }
    }
    rulebuilder.FunctorLiteral(getRelation, fields, this)
  }

}

abstract class FilterSpec(name: String, signature: List[Type])
  extends AbstractFunctorSpec

case class MakeList(signature: List[Type]) extends FunctorSpec {
  def name : String = "makeList"
  def inputIndices: List[Int] = List(0,1)
  def outputIndex: Int = 2
  require(signature.size == 3)
  require(signature(0)==signature(1))

  def literalToString(literal: Literal): String = {
    require(literal.relation == this.getRelation)
    val output = getOutput(literal)
    val inputs = getInputs(literal)
    s"${output}=cat(${inputs.mkString(",")})"
  }

}
object MakeList {
  def allInstances(problem: Problem): Set[AbstractFunctorSpec] = {
    /** Look for type x and type xList */
    val inputTypes = problem.inputTypes
    val outputTypes = problem.outputTypes
    val allTypes = inputTypes ++ outputTypes

    val listTypes: Set[Type] = outputTypes.filter(AbstractFunctorSpec.isListType)
    listTypes.map { lt =>
      val nodeName = AbstractFunctorSpec.getNodeName(lt)
      val _nodeTypes: Set[Type] = allTypes.filter(_.name == nodeName)
      require(_nodeTypes.size==1)
      val nodeType: Type = _nodeTypes.toList.head
      MakeList(List(nodeType, nodeType, lt))
    }
  }
}

case class PrependList(signature: List[Type]) extends FunctorSpec {
  def name: String = "safePrepend"
  def inputIndices: List[Int] = List(0,1)
  def outputIndex: Int = 2

  require(signature.size == 3)
  require(signature(1)==signature(2))
  require(AbstractFunctorSpec.isListType(signature(1)))
  require(AbstractFunctorSpec.isListType(signature(2)))

  def literalToString(literal: Literal): String = {
    require(literal.relation == this.getRelation)
    val output = getOutput(literal)
    val inputs = getInputs(literal)

    val h = inputs(0)
    val tail = inputs(1)

    s"${output}=cat(${h}, ${tail}), !contains(as(${h},symbol), as(${tail},symbol))"
  }
}
object PrependList {
  def allInstances(problem: Problem): Set[AbstractFunctorSpec] = {
    /** Look for type x and type xList */
    val inputTypes = problem.inputTypes
    val outputTypes = problem.outputTypes
    val allTypes = inputTypes ++ outputTypes

    val listTypes: Set[Type] = outputTypes.filter(AbstractFunctorSpec.isListType)
    listTypes.map { lt =>
      val nodeName = AbstractFunctorSpec.getNodeName(lt)
      val _nodeTypes: Set[Type] = allTypes.filter(_.name == nodeName)
      require(_nodeTypes.size==1)
      val nodeType: Type = _nodeTypes.toList.head
      PrependList(List(nodeType, lt, lt))
    }
  }
}

case class Add(signature: List[Type]) extends FunctorSpec {
  def name: String = "add"
  def inputIndices: List[Int] = List(0,1)
  def outputIndex: Int = 2

  require(signature.size == 3)
  require(signature.toSet.size == 1)
  require(signature.forall(_.isInstanceOf[NumberType]))

  def literalToString(literal: Literal): String = {
    require(literal.relation == this.getRelation)
    val output = getOutput(literal)
    val inputs = getInputs(literal)
    require(inputs.size==2)
    s"${output}=${inputs(0)}+${inputs(1)}"
  }
}
object Add {
  def allInstances(problem: Problem): Set[AbstractFunctorSpec] = {
    /** Look for Number types */
    val inputTypes = problem.inputTypes
    val outputTypes = problem.outputTypes
    val allTypes = inputTypes ++ outputTypes

    val numberTypes: Set[Type] = outputTypes.filter(_.isInstanceOf[NumberType])
    numberTypes.map { nt =>
      require(inputTypes.contains(nt))
      Add(List(nt, nt, nt))
    }
  }
}

case class Min(signature: List[Type]) extends FunctorSpec {
  def name: String = "min"
  def inputIndices: List[Int] = List(0,1)
  def outputIndex: Int = 2

  require(signature.size == 3)
  require(signature.toSet.size == 1)
  require(signature.forall(_.isInstanceOf[NumberType]))

  def literalToString(literal: Literal): String = {
    require(literal.relation == this.getRelation)
    val output = getOutput(literal)
    val inputs = getInputs(literal)
    require(inputs.size==2)
    s"${output}=min(${inputs(0)},${inputs(1)})"
  }
}
object Min {
  def allInstances(problem: Problem): Set[AbstractFunctorSpec] = {
    /** Look for Number types */
    val inputTypes = problem.inputTypes
    val outputTypes = problem.outputTypes
    val allTypes = inputTypes ++ outputTypes

    val numberTypes: Set[Type] = outputTypes.filter(_.isInstanceOf[NumberType])
    numberTypes.map { nt =>
      require(inputTypes.contains(nt))
      Min(List(nt, nt, nt))
    }
  }
}

case class Max(signature: List[Type]) extends FunctorSpec {
  def name: String = "max"
  def inputIndices: List[Int] = List(0,1)
  def outputIndex: Int = 2

  require(signature.size == 3)
  require(signature.toSet.size == 1)
  require(signature.forall(_.isInstanceOf[NumberType]))

  def literalToString(literal: Literal): String = {
    require(literal.relation == this.getRelation)
    val output = getOutput(literal)
    val inputs = getInputs(literal)
    require(inputs.size==2)
    s"${output}=max(${inputs(0)},${inputs(1)})"
  }
}
object Max {
  def allInstances(problem: Problem): Set[AbstractFunctorSpec] = {
    /** Look for Number types */
    val inputTypes = problem.inputTypes
    val outputTypes = problem.outputTypes
    val allTypes = inputTypes ++ outputTypes

    val numberTypes: Set[Type] = outputTypes.filter(_.isInstanceOf[NumberType])
    numberTypes.map { nt =>
      require(inputTypes.contains(nt))
      Max(List(nt, nt, nt))
    }
  }
}

case class AppendList(signature: List[Type]) extends FunctorSpec {
  def name: String = "safeAppend"
  def inputIndices: List[Int] = List(0,1)
  def outputIndex: Int = 2

  require(signature.size == 3)
  require(signature(0)==signature(2))
  require(AbstractFunctorSpec.isListType(signature(0)))
  require(AbstractFunctorSpec.isListType(signature(2)))

  def literalToString(literal: Literal): String = {
    require(literal.relation == this.getRelation)
    val output = getOutput(literal)
    val inputs = getInputs(literal)

    val h = inputs(0)
    val tail = inputs(1)

    s"${output}=cat(${h}, ${tail}), !contains(as(${tail},symbol), as(${h},symbol))"
  }
}
object AppendList {
  def allInstances(problem: Problem): Set[AbstractFunctorSpec] = {
    /** Look for type x and type xList */
    val inputTypes = problem.inputTypes
    val outputTypes = problem.outputTypes
    val allTypes = inputTypes ++ outputTypes

    val listTypes: Set[Type] = outputTypes.filter(AbstractFunctorSpec.isListType)
    listTypes.map { lt =>
      val nodeName = AbstractFunctorSpec.getNodeName(lt)
      val _nodeTypes: Set[Type] = allTypes.filter(_.name == nodeName)
      require(_nodeTypes.size==1)
      val nodeType: Type = _nodeTypes.toList.head
      AppendList(List(lt, nodeType, lt))
    }
  }
}
