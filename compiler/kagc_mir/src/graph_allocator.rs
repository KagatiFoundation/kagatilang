use std::collections::HashMap;
use std::collections::HashSet;

use crate::value::IrValueId;

pub struct GraphColoringAllocator {
    pub physical_registers: Vec<String>,
}

#[derive(Debug)]
pub struct AllocationResult {
    pub mapping: HashMap<IrValueId, String>,
    pub spills: HashSet<IrValueId>,
}

impl GraphColoringAllocator {
    pub fn new(phys_regs: Vec<String>) -> Self {
        Self { physical_registers: phys_regs }
    }

    pub fn allocate(&self, graph: crate::interference_graph::InterferenceGraph) -> AllocationResult {
        let k = self.physical_registers.len();
        
        let mut select_stack: Vec<IrValueId> = Vec::new();
        let mut removed_nodes: HashSet<IrValueId> = HashSet::new();
        
        let mut working_degrees = graph.degrees.clone();

        let mut changed = true;
        while changed {
            changed = false;
            
            let candidate = working_degrees.iter()
                .find(|(&val, &deg)| !removed_nodes.contains(&val) && deg < k)
                .map(|(&val, _)| val);

            if let Some(node_to_remove) = candidate {
                removed_nodes.insert(node_to_remove);
                select_stack.push(node_to_remove);
                
                if let Some(neighbors) = graph.adj_list.get(&node_to_remove) {
                    for neighbor in neighbors {
                        if !removed_nodes.contains(neighbor) {
                            if let Some(deg) = working_degrees.get_mut(neighbor) {
                                if *deg > 0 { *deg -= 1; }
                            }
                        }
                    }
                }
                changed = true;
            }
        }

        let mut spills = HashSet::new();
        
        for &node in graph.adj_list.keys() {
            if !removed_nodes.contains(&node) {
                spills.insert(node);
                removed_nodes.insert(node); 
            }
        }

        let mut mapping: HashMap<IrValueId, String> = HashMap::new();

        while let Some(node) = select_stack.pop() {
            let mut used_colors = HashSet::new();
            if let Some(neighbors) = graph.adj_list.get(&node) {
                for neighbor in neighbors {
                    if let Some(assigned_reg) = mapping.get(neighbor) {
                        used_colors.insert(assigned_reg.clone());
                    }
                }
            }

            let available_color = self.physical_registers.iter()
                .find(|&reg| !used_colors.contains(reg));

            match available_color {
                Some(reg) => {
                    mapping.insert(node, reg.clone());
                }
                None => {
                    spills.insert(node);
                }
            }
        }

        AllocationResult { mapping, spills }
    }
}