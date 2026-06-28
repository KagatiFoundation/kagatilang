#![allow(clippy::new_without_default)]

use crate::value::IrValueId;
use crate::function::IrFunction;
use crate::block::BlockId;
use crate::block::BlockLiveness;

use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Debug)]
pub struct InterferenceGraph {
    pub adj_list: HashMap<IrValueId, HashSet<IrValueId>>,
    pub degrees: HashMap<IrValueId, usize>,
}

impl InterferenceGraph {
    pub fn new() -> Self {
        Self {
            adj_list: HashMap::new(),
            degrees: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, val: IrValueId) {
        self.adj_list.entry(val).or_default();
        self.degrees.entry(val).or_insert(0);
    }

    pub fn add_edge(&mut self, u: IrValueId, v: IrValueId) {
        if u == v {
            return; 
        }

        if self.adj_list.entry(u).or_default().insert(v) {
            *self.degrees.entry(u).or_insert(0) += 1;
        }

        if self.adj_list.entry(v).or_default().insert(u) {
            *self.degrees.entry(v).or_insert(0) += 1;
        }
	}

    pub fn build(func: &IrFunction, global_liveness: &HashMap<BlockId, BlockLiveness>) -> Self {
        let mut graph = Self::new();

        for block in func.blocks.values() {
            for inst in &block.instructions {
                for def in inst.defs() { graph.add_node(def); }
                for u in inst.uses() { graph.add_node(u); }
            }
        }

        for (block_id, block) in &func.blocks {
            let liveness = &global_liveness[block_id];
            
            let mut currently_live = liveness.live_out.clone();

            for inst in block.instructions.iter().rev() {
                let defs = inst.defs();
                let uses = inst.uses();

                for def in &defs {
                    for live_val in &currently_live {
                        graph.add_edge(*def, *live_val);
                    }
                }

                for def in defs {
                    currently_live.remove(&def);
                }

                for u in uses {
                    currently_live.insert(u);
                }
            }
        }

        graph
    }
}