#define move_to_user_mode() \
__asm__ ("movl %%esp,%%eax\n\t" \
	"pushl $0x17\n\t" /* NOTE: SS, 0特权栈的代码段 */ \
	"pushl %%eax\n\t" /* NOTE: ESP, 就是原本的ESP */ \
	"pushfl\n\t" /* NOTE: EFLAGS */ \
	"pushl $0x0f\n\t" /* NOTE: CS */ \
	"pushl $1f\n\t" /* NOTE: EIP, $1f是下面1标签行地址的高位. 因此模拟压栈及中断返回后就是执行的下面几行 */ \
	"iret\n" /* NOTE: 利用中断时硬件自动压栈, 返回时自动弹栈的特性, 上面几行手动设置几个寄存器后触发中断返回 */ \
	/* 这行以上是0特权执行, 下面是3特权级 */ \
	"1:\tmovl $0x17,%%eax\n\t" \
	"movw %%ax,%%ds\n\t" \
	"movw %%ax,%%es\n\t" \
	"movw %%ax,%%fs\n\t" \
	"movw %%ax,%%gs" \
	:::"ax")

#define sti() __asm__ ("sti"::)
#define cli() __asm__ ("cli"::)
#define nop() __asm__ ("nop"::)

#define iret() __asm__ ("iret"::)

/* NOTE: eax, edx都是4B, 先把值赋到两个寄存器, 然后直接把两个寄存器的值赋值到指定地址, 更新描述符
   (一个描述符8B). 无法直接由内存mv到内存
   edx初值高4位即GDT地址
   教材图2-10
 */
#define _set_gate(gate_addr,type,dpl,addr) \
__asm__ ("movw %%dx,%%ax\n\t" \
	"movw %0,%%dx\n\t" \
	"movl %%eax,%1\n\t" \
	"movl %%edx,%2" \
	: \
	: "i" ((short) (0x8000+(dpl<<13)+(type<<8))), \
	"o" (*((char *) (gate_addr))), \
	"o" (*(4+(char *) (gate_addr))), \
	"d" ((char *) (addr)),"a" (0x00080000)) // NOTE: d给edx初值, a给eax初值

// NOTE: 中断门
#define set_intr_gate(n,addr) \
	_set_gate(&idt[n],14,0,addr)

// NOTE: 陷阱门
#define set_trap_gate(n,addr) \
	_set_gate(&idt[n],15,0,addr) // NOTE: 0特权, 因此无法由用户主动唤起硬中断/硬异常

#define set_system_gate(n,addr) \
	_set_gate(&idt[n],15,3,addr) // NOTE: 这里是3特权, 因为是在**用户态**使用int0x80翻转到内核态

#define _set_seg_desc(gate_addr,type,dpl,base,limit) {\
	*(gate_addr) = ((base) & 0xff000000) | \
		(((base) & 0x00ff0000)>>16) | \
		((limit) & 0xf0000) | \
		((dpl)<<13) | \
		(0x00408000) | \
		((type)<<8); \
	*((gate_addr)+1) = (((base) & 0x0000ffff)<<16) | \
		((limit) & 0x0ffff); }

#define _set_tssldt_desc(n,addr,type) \
__asm__ ("movw $104,%1\n\t" \
	"movw %%ax,%2\n\t" \
	"rorl $16,%%eax\n\t" \
	"movb %%al,%3\n\t" \
	"movb $" type ",%4\n\t" \
	"movb $0x00,%5\n\t" \
	"movb %%ah,%6\n\t" \
	"rorl $16,%%eax" \
	::"a" (addr), "m" (*(n)), "m" (*(n+2)), "m" (*(n+4)), \
	 "m" (*(n+5)), "m" (*(n+6)), "m" (*(n+7)) \
	)

#define set_tss_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x89")
#define set_ldt_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x82")
