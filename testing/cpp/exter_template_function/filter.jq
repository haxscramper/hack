def main: .traceEvents[] | 
 select((.name | contains("template")) or ((.args // {}).detail // "" | contains("template"))) 
 .name + " for " + .args.detail
 ;
 