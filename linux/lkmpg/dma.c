// SPDX-License-Identifier: GPL-2.0
/*
 * dma.c - Minimal DMA API demonstration using a synthetic platform device.
 *
 * This module does not drive real hardware. It exists to show the shape of a
 * modern DMA-capable probe path: set the DMA mask, allocate coherent memory,
 * create a streaming mapping, and tear it all down cleanly.
 */

#include <linux/dma-mapping.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/platform_device.h>
#include <linux/slab.h>
#include <linux/version.h>

static unsigned int coherent_size = PAGE_SIZE;
module_param(coherent_size, uint, 0444);

static unsigned int streaming_size = PAGE_SIZE;
module_param(streaming_size, uint, 0444);
MODULE_PARM_DESC(streaming_size, "Size of streaming DMA buffer");

MODULE_DESCRIPTION("LKMPG DMA API demo module");
MODULE_LICENSE("GPL");
