/* graph-for-funcs.scala

   This script returns a Json representation of the graph resulting in combining the
   AST, CGF, and PDG for each method contained in the currently loaded CPG.

   Input: A valid CPG
   Output: Json

   Running the Script
   ------------------
   see: README.md

   The JSON generated has the following keys:

   "functions": Array of all methods contained in the currently loaded CPG
     |_ "function": Method name as String
     |_ "id": Method id as String (String representation of the underlying Method node)
     |_ "AST": see ast-for-funcs script
     |_ "CFG": see cfg-for-funcs script
     |_ "PDG": see pdg-for-funcs script
 */

import scala.jdk.CollectionConverters._
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Encoder, Json}

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.{Local, MethodParameterIn}
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import overflowdb._
import overflowdb.traversal._

final case class GraphForFuncsFunction(function: String,
                                       file: String,
                                       id: String,
                                       AST: List[nodes.AstNode],
                                       CFG: List[nodes.AstNode],
                                       PDG: List[nodes.AstNode])
final case class GraphForFuncsResult(functions: List[GraphForFuncsFunction])

implicit val encodeEdge: Encoder[Edge] = {
  (edge: Edge) =>
    Json.obj(
      ("id", Json.fromString(edge.toString)),
      ("in", Json.fromString(edge.inNode.toString)),
      ("out", Json.fromString(edge.outNode.toString))
    )
}

implicit val encodeNode: Encoder[nodes.AstNode] = {
  (node: nodes.AstNode) =>
    Json.obj(
      ("id", Json.fromString(node.toString)),
      ("edges",
        Json.fromValues((node.inE("AST", "CFG").asScala.l ++ node.outE("AST", "CFG").asScala.l).map(_.asJson))),
      ("properties", Json.fromValues(node.propertiesMap().asScala.toList.map { case (key, value) =>
        Json.obj(
          ("key", Json.fromString(key)),
          ("value", Json.fromString(value.toString))
        )
      }))
    )
}

implicit val encodeFuncFunction: Encoder[GraphForFuncsFunction] = deriveEncoder
implicit val encodeFuncResult: Encoder[GraphForFuncsResult] = deriveEncoder


@main def main(): Json = {
  GraphForFuncsResult(
    cpg.method.map { method =>
      val methodName = method.fullName
      val methodId = method.toString
      val methodFile = method.location.filename
      val methodVertex = method //TODO MP drop as soon as we have the remainder of the below in ODB graph api

      val astChildren = method.astMinusRoot.l
      val cfgChildren = method.out(EdgeTypes.CONTAINS).asScala.collect { case node: nodes.CfgNode => node }.toList

      val local = new Traversal(
        methodVertex
          .out(EdgeTypes.CONTAINS).asScala)
          .hasLabel(NodeTypes.BLOCK)
          .out(EdgeTypes.AST)
          .hasLabel(NodeTypes.LOCAL)
          .cast[Local]
      val sink = local.evalType(".*").referencingIdentifiers.dedup
      val source = new Traversal(methodVertex.out(EdgeTypes.CONTAINS).asScala).hasLabel(NodeTypes.CALL).cast[nodes.Call].nameNot("<operator>.*").dedup

      val pdgChildren = sink
        .reachableByFlows(source)
        .l
        .flatMap { path =>
          path.elements
            .map {
              case cfgNode@(_: MethodParameterIn) => cfgNode.start.method.head
              case cfgNode => cfgNode
            }
        }
        .filter(_.toString != methodId)

      GraphForFuncsFunction(methodName, methodFile, methodId, astChildren, cfgChildren, pdgChildren.distinct)
    }.l
  ).asJson
}
