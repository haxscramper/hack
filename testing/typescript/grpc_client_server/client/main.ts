import { credentials } from "@grpc/grpc-js";
import { org } from "./generated/org.ts";

const client = new org.OrgServiceClient(
  "localhost:50051",
  credentials.createInsecure()
);

async function example() {
  // Get root handle
  const root = await client.GetRoot(new org.Empty(), (err, root_res) => {
    console.log("Got root request response");
  });
  
  // Get kind
  const kind = await client.getKind({ handle: root.handle });
  
  // Get subnodes count
  const count = await client.getSubnodeCount({ handle: root.handle });
  
  // Get specific subnode
  const subnode = await client.getSubnode({ 
    handle: root.handle, 
    index: 0 
  });
}

example().catch(console.error);
