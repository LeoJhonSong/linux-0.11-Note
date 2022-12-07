/*
 *  linux/boot/head.s
 *  # NOTE: bootsect.s与setup.s采用近似intel的汇编语言语法, 使用intel8086汇编编译器as86,
 *  # NOTE: 而head.s采用AT&T汇编语言语法, 使用GNU汇编编译器as, 是因为对于intelx86系列处理器,
 *  # NOTE: as仅支持i386及之后的处理器, 且不支持生成运行在实模式下的程序
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
.text
.globl _idt,_gdt,_pg_dir,_tmp_floppy_area #NOTE: 所有有前缀下划线的都是在C语言中定义的 (C语言中对应变量没有前缀下划线) (在kernel/sched.c, 72行)
_pg_dir: # NOTE: 页目录表有1k个页表. 一个页表有1k项页, 每页对应4k内存. 因此一个页表最多管理4M内存, 一个页目录表最多管理4G内存. (图1-37). 由CR3 (页目录表基址寄存器) 指向页目录表基址, **记录的是物理地址**
startup_32:
	movl $0x10,%eax
	# NOTE: 下面5行在下几行有重复, 似乎无用
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%esp
	call setup_idt # NOTE: 中断描述符表在很后面才建立, 之所以这么早建立IDT, 可能是当时开发时增量式开发, 在这里先测试一下IDT实现是否成功
	call setup_gdt
	movl $0x10,%eax		# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was alread # NOTE: DS, ES, FS, GS都变为内核数据段, 且基址一样 (段重叠了), 都是值为0x10的段选择子指向的GDT表第2项
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%es # NOTE: 将esp指向_stack_start这个值, esp是栈顶指针 (stack_start定义在C语言中, kernel/sched.c,L72)p
	xorl %eax,%eax
1:	incl %eax		# check that A20 really IS enabled # NOTE: 越是偏硬件的代码越要注重自检. 此处检测原理在P34图1-33
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000
	je 1b
/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */
	movl %cr0,%eax		# check math chip #NOTE: 检测是否存在数学协处理器X87
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f			/* no coprocessor: have to set bits */
	movl %cr0,%eax
	xorl $6,%eax		/* reset MP, set EM */
	movl %eax,%cr0
	ret
.align 2
1:	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
# NOTE: IDT中一部分为指向GDT中一个段描述符, 为中断服务程序的段基址. 另一部分为指定中断服务程序在所在段上的偏移, 特权等
setup_idt:
	lea ignore_int,%edx # NOTE: 中断程序偏移地址
	movl $0x00080000,%eax
	movw %dx,%ax		/* selector = 0x0008 = cs */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea _idt,%edi
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	lidt idt_descr
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
# NOTE: 这都是内核页表 (图1-37)
.org 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
_tmp_floppy_area:
	.fill 1024,1,0

# NOTE: 这段到_idt标签前的代码刚好没有在head.s执行完后被覆盖掉
after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	# NOTE: 这里压栈后在跳转到setup_paging执行完然后L224的ret后会进入C语言内核主函数.
	# NOTE: 不用call只是一个逻辑问题, 内核主函数应当是最底层的, 被调用不合理, 应当是从主函数退出后继续操作的面向过程式
	pushl $_main
	jmp setup_paging
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call _printk # NOTE: 此时还没有建立文件系统, 无法使用printf. 在系统完全建立后也有使用printk的情况: 在内核中进行输出. 此处打印149行字符串
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret # NOTE: 中断返回. 能看出这上面一段是关于中断的代码


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
# NOTE: 32位线性空间最多寻址1M页. 高20位用于寻址, 由于硬件限制访问页的基址必然为4k整数倍, 因此低12位必然是0.
# NOTE: 因此此处32位的低12位用于状态位, 低三位由高到低为U/S, R/W, P存在位
# NOTE: 存在位可以指示是否已为线性内存分配了物理内存. 当运行到存在位为0的地址时报缺页中断申请分配内存
.align 2
setup_paging:
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */
	cld;rep;stosl
	# NOTE: 7 (0b111) 表示用户u, 读写rw, 存在p. r/w位为0表示只读
	# NOTE: 设为3特权级, 这样move_touser_mode()后与原本0特权栈重合的进程0的用户栈才能用
	movl $pg0+7,_pg_dir		/* set present bit/user r/w */ # NOTE: 放到内存起始位置处
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,_pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,_pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b
	xorl %eax,%eax		/* pg_dir is at 0x0000 */
	movl %eax,%cr3		/* cr3 - page directory start */ # NOTE: 通过由CR3指向页目录表基址给出当前线性地址空间. 汇编中都是mov但是操作CR3等特殊寄存器的语句汇编出的指令opcode与普通mov语句的opcode不同
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0		/* set paging (PG) bit */ # NOTE: 打开分页
	ret			/* this also flushes prefetch-queue */

.align 2
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries
	.long _idt
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long _gdt		# magic number, but it works for me :^)

	.align 3
_idt:	.fill 256,8,0		# idt is uninitialized

# NOTE: 后续C语言内核代码中的gdt列表变量
_gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	/* 16Mb */
	.quad 0x00c0920000000fff	/* NOTE: 16Mb, 数据段 */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */
