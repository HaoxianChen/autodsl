package synthesis.util

import java.nio.file.{Path, Paths}

import synthesis.util.Misc.writeFile

/** Example Convertor
 * Input: path to example file, which aggregates all examples,
 *        in tuple format.
 *        This file also specify static configurations.
 * Output: Write output file for each relation.
 *        Add instance ID for each tuple.
 *        Dispatch the static configuration to each example instance.
 * */
class ExampleConvertor {
  private val exampleFileSuffix: String = "examples"
  def parse(dir: String): Unit = {
    val exampleFile: Path = {
      val allFiles = Misc.getListOfFiles(dir)
      val f: List[String] = allFiles.filter(_.endsWith(exampleFileSuffix))
      require(f.size==1, s"Should have only one example file.")
      Paths.get(dir, f.head)
    }
    val inputString = Misc.fileToString(exampleFile.toString)
    val parser = new ExampleParser()
    val (staticConfigs, examples) = parser.parseAll(parser.examples, inputString).get

    // add static configs
    val examplesWithStaticConfig = examples.map(_++staticConfigs)

    // add instance Ids
    val examplesWithId: List[List[(String, List[Int])]] = examplesWithStaticConfig.zipWithIndex.map {
      case (tupleSet,i) => tupleSet.map {
        case (name, fields) => (name, fields:+i)
      }
    }

    // Write examples to files
    val examplesByRel: Map[String, List[List[Int]]] = examplesWithId.flatten.groupBy(_._1).map {
      case (name, tupleList) => name -> tupleList.map(_._2)
    }


    examplesByRel.foreach {
      case (relName, tupleList) => {
        val filePath = Paths.get(dir, s"$relName.facts")
        val fileString = tupleList.map(_.mkString(sep="\t")).mkString(sep="\n")
        writeFile(fileString, filePath)
      }
    }
  }
}
